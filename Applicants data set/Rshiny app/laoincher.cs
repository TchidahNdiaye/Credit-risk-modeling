using System;
using System.Diagnostics;
using System.IO;
using System.Net;
using System.Net.Sockets;
using System.Threading;
using System.Windows.Forms;

class Program
{
    // Mutex global (unique sur la machine)
    static Mutex _mutex;

    // Stop signal
    static ManualResetEvent _stopSignal = new ManualResetEvent(false);

    [STAThread]
    static void Main()
    {
        string exeDir = AppDomain.CurrentDomain.BaseDirectory.TrimEnd('\\');
        string appDir = Path.Combine(exeDir, "app");
        string rExe   = Path.Combine(exeDir, "R", "bin", "R.exe");

        // Local dirs
        string localBase = Path.Combine(
            Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData),
            "calculetteRWA"
        );
        string logDir = Path.Combine(localBase, "logs");
        string tmpDir = Path.Combine(localBase, "tmp");
        Directory.CreateDirectory(logDir);
        Directory.CreateDirectory(tmpDir);

        string logFile     = Path.Combine(logDir, "startup.log");
        string pidFile     = Path.Combine(localBase, "shiny.pid");
        string portFile    = Path.Combine(localBase, "shiny.port");
        string ctrlPortFile= Path.Combine(localBase, "control.port");
        string ctrlTokFile = Path.Combine(localBase, "control.token");

        void Log(string s) => File.AppendAllText(logFile, $"[{DateTime.Now:yyyy-MM-dd HH:mm:ss}] {s}\n");

        Log("=== START launch ===");

        try
        {
            // ---------------------------
            // 1) Mutex global (no multi-instance)
            // ---------------------------
            bool createdNew;
            _mutex = new Mutex(initiallyOwned: true, name: @"Global\CalculetteRWA_Mutex", createdNew: out createdNew);

            if (!createdNew)
            {
                // instance already running -> open browser on existing port if possible
                int existingPort = TryReadInt(portFile);
                string url = existingPort > 0 ? $"http://127.0.0.1:{existingPort}/" : null;

                if (!string.IsNullOrEmpty(url))
                {
                    Process.Start(new ProcessStartInfo { FileName = url, UseShellExecute = true });
                }

                MessageBox.Show(
                    "calculetteRWA est déjà en cours d'exécution.\n" +
                    (url != null ? $"Ouverture: {url}" : "Impossible de déterminer le port."),
                    "Instance déjà active",
                    MessageBoxButtons.OK,
                    MessageBoxIcon.Information
                );
                return;
            }

            // ---------------------------
            // 2) Vérifs fichiers essentiels
            // ---------------------------
            if (!File.Exists(rExe)) throw new FileNotFoundException("R.exe introuvable: " + rExe);
            if (!File.Exists(Path.Combine(appDir, "app.R"))) throw new FileNotFoundException("app/app.R introuvable: " + Path.Combine(appDir, "app.R"));

            // Libraries: prefer root/library then fallback R/library
            string libApp = Path.Combine(exeDir, "library");
            string libR   = Path.Combine(exeDir, "R", "library");
            string libUse = Directory.Exists(libApp) ? libApp : libR;

            // ---------------------------
            // 3) Port dynamique Shiny + port dynamique control endpoint
            // ---------------------------
            int shinyPort = GetFreePort();
            int ctrlPort  = GetFreePort();
            string host = "127.0.0.1";
            string token = Guid.NewGuid().ToString("N"); // token sécurisé

            File.WriteAllText(portFile, shinyPort.ToString());
            File.WriteAllText(ctrlPortFile, ctrlPort.ToString());
            File.WriteAllText(ctrlTokFile, token);

            Log($"Shiny port: {shinyPort} (port file: {portFile})");
            Log($"Control port: {ctrlPort} (control.port file: {ctrlPortFile})");
            Log($"Control token written (control.token)");

            // ---------------------------
            // 4) Démarrer serveur de contrôle local (endpoint stop)
            // ---------------------------
            var control = new ControlServer(ctrlPort, token, Log);
            control.OnStopRequested += () => _stopSignal.Set();
            control.Start();

            // ---------------------------
            // 5) Lancer R + Shiny (launch.browser=FALSE)
            //    et injecter CTRL_PORT/CTRL_TOKEN dans l'env pour l'IHM Stop
            // ---------------------------
            string appDirEsc = appDir.Replace("\\", "/");
            string libUseEsc = libUse.Replace("\\", "/");

            string rCommand =
                "tryCatch({" +
                $"  .libPaths(c('{libUseEsc}'));" +
                $"  options(shiny.port={shinyPort}, shiny.host='{host}');" +
                $"  shiny::runApp('{appDirEsc}', launch.browser=FALSE);" +
                "}, error=function(e){" +
                "  message('ERROR: ', conditionMessage(e));" +
                "  quit(status=1);" +
                "})";

            var psi = new ProcessStartInfo
            {
                FileName = rExe,
                Arguments = $"-e \"{rCommand}\"",
                WorkingDirectory = exeDir,
                UseShellExecute = false,
                CreateNoWindow = true,
                RedirectStandardOutput = true,
                RedirectStandardError = true
            };

            psi.EnvironmentVariables["R_LIBS_USER"] = libUse;
            psi.EnvironmentVariables["R_LIBS"] = libUse;

            psi.EnvironmentVariables["TMPDIR"] = tmpDir;
            psi.EnvironmentVariables["TEMP"] = tmpDir;
            psi.EnvironmentVariables["TMP"] = tmpDir;

            // variables pour l'IHM Stop
            psi.EnvironmentVariables["CALC_RWA_CTRL_PORT"]  = ctrlPort.ToString();
            psi.EnvironmentVariables["CALC_RWA_CTRL_TOKEN"] = token;

            var p = new Process();
            p.StartInfo = psi;
            p.EnableRaisingEvents = true;

            p.OutputDataReceived += (sender, e) => { if (e.Data != null) Log("R> " + e.Data); };
            p.ErrorDataReceived  += (sender, e) => { if (e.Data != null) Log("RERR> " + e.Data); };

            if (!p.Start()) throw new Exception("Impossible de démarrer le processus R.");
            p.BeginOutputReadLine();
            p.BeginErrorReadLine();

            File.WriteAllText(pidFile, p.Id.ToString());
            Log($"PID: {p.Id} (pid file: {pidFile})");

            // ---------------------------
            // 6) Healthcheck Shiny
            // ---------------------------
            string url = $"http://{host}:{shinyPort}/";
            Log("Healthcheck URL: " + url);

            bool ok = WaitForHttpAlive(url, timeoutMs: 45000, pollMs: 600);
            if (!ok)
            {
                if (p.HasExited) throw new Exception("R/Shiny s'est arrêté immédiatement. Voir log: " + logFile);
                throw new Exception("Shiny ne répond pas après 45s. Voir log: " + logFile);
            }

            // Ouvrir navigateur
            Process.Start(new ProcessStartInfo { FileName = url, UseShellExecute = true });
            Log("Launch OK (browser opened).");

            // ---------------------------
            // 7) Boucle de vie : attendre stop demandé OU fin de R
            // ---------------------------
            while (true)
            {
                if (p.HasExited)
                {
                    Log("R process exited.");
                    break;
                }

                if (_stopSignal.WaitOne(500))
                {
                    Log("Stop requested. Killing R process...");
                    try
                    {
                        if (!p.HasExited)
                        {
                            p.Kill(true);
                            p.WaitForExit(8000);
                        }
                    }
                    catch (Exception ex)
                    {
                        Log("Kill failed: " + ex.Message);
                    }
                    break;
                }
            }

            // Cleanup
            control.Stop();
            SafeDelete(pidFile);
            SafeDelete(portFile);
            SafeDelete(ctrlPortFile);
            SafeDelete(ctrlTokFile);

            Log("Clean exit.");
        }
        catch (Exception ex)
        {
            File.AppendAllText(logFile, $"[{DateTime.Now:yyyy-MM-dd HH:mm:ss}] FAILED: {ex}\n");

            MessageBox.Show(
                "Impossible de démarrer calculetteRWA.\n\n" +
                ex.Message + "\n\nLog: " + logFile,
                "Erreur lancement calculetteRWA",
                MessageBoxButtons.OK,
                MessageBoxIcon.Error
            );
        }
        finally
        {
            try { _mutex?.ReleaseMutex(); } catch { }
            try { _mutex?.Dispose(); } catch { }
        }
    }

    static int GetFreePort()
    {
        var listener = new TcpListener(IPAddress.Loopback, 0);
        listener.Start();
        int port = ((IPEndPoint)listener.LocalEndpoint).Port;
        listener.Stop();
        return port;
    }

    static bool WaitForHttpAlive(string url, int timeoutMs, int pollMs)
    {
        var sw = Stopwatch.StartNew();
        while (sw.ElapsedMilliseconds < timeoutMs)
        {
            try
            {
                var req = (HttpWebRequest)WebRequest.Create(url);
                req.Method = "GET";
                req.Timeout = 2000;

                using (var resp = (HttpWebResponse)req.GetResponse())
                {
                    int code = (int)resp.StatusCode;
                    if (code >= 200 && code < 500) return true; // server alive
                }
            }
            catch { }
            Thread.Sleep(pollMs);
        }
        return false;
    }

    static int TryReadInt(string path)
    {
        try
        {
            if (!File.Exists(path)) return -1;
            var t = File.ReadAllText(path).Trim();
            if (int.TryParse(t, out int v)) return v;
            return -1;
        }
        catch { return -1; }
    }

    static void SafeDelete(string path)
    {
        try { if (File.Exists(path)) File.Delete(path); } catch { }
    }
}

// --------------------------------------------
// Local control server: http://127.0.0.1:<port>/stop?token=<token>
// loopback only, token required
// --------------------------------------------
class ControlServer
{
    private readonly int _port;
    private readonly string _token;
    private readonly Action<string> _log;
    private HttpListener _listener;
    private Thread _thread;
    private volatile bool _running;

    public event Action OnStopRequested;

    public ControlServer(int port, string token, Action<string> log)
    {
        _port = port;
        _token = token;
        _log = log;
    }

    public void Start()
    {
        _listener = new HttpListener();
        _listener.Prefixes.Add($"http://127.0.0.1:{_port}/");
        _listener.Start();

        _running = true;
        _thread = new Thread(ListenLoop);
        _thread.IsBackground = true;
        _thread.Start();

        _log($"ControlServer started on 127.0.0.1:{_port}");
    }

    public void Stop()
    {
        _running = false;
        try { _listener?.Stop(); } catch { }
        try { _listener?.Close(); } catch { }
        _log("ControlServer stopped.");
    }

    private void ListenLoop()
    {
        while (_running)
        {
            try
            {
                var ctx = _listener.GetContext(); // blocking
                Handle(ctx);
            }
            catch
            {
                // listener stopped or transient error
            }
        }
    }

    private void Handle(HttpListenerContext ctx)
    {
        try
        {
            var req = ctx.Request;
            var res = ctx.Response;

            string path = req.Url.AbsolutePath ?? "/";
            string token = req.QueryString["token"] ?? "";

            if (path.Equals("/ping", StringComparison.OrdinalIgnoreCase))
            {
                Write(res, 200, "OK");
                return;
            }

            if (path.Equals("/stop", StringComparison.OrdinalIgnoreCase))
            {
                if (token != _token)
                {
                    Write(res, 401, "UNAUTHORIZED");
                    return;
                }

                Write(res, 200, "STOPPING");
                _log("Stop endpoint called (authorized).");
                OnStopRequested?.Invoke();
                return;
            }

            Write(res, 404, "NOT_FOUND");
        }
        catch
        {
            // ignore
        }
    }

    private void Write(HttpListenerResponse res, int code, string body)
    {
        try
        {
            byte[] buf = System.Text.Encoding.UTF8.GetBytes(body);
            res.StatusCode = code;
            res.ContentType = "text/plain";
            res.OutputStream.Write(buf, 0, buf.Length);
            res.OutputStream.Close();
        }
        catch { }
    }
}