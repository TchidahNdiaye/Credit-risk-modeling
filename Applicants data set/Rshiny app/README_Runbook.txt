RUNBOOK - calculetteRWA (Portable Windows)

1) Démarrage
- Double-cliquer sur run_calculetteRWA.bat
- L'application s'ouvre dans le navigateur: http://127.0.0.1:3838

2) Arrêt
- Fermer le navigateur
- Si besoin: stop_calculetteRWA.bat

3) Logs
- Logs utilisateur: %LOCALAPPDATA%\calculetteRWA\logs\startup.log

4) Librairies
- Priorité 1: calculetteRWA\library\ (packages applicatifs)
- Priorité 2: calculetteRWA\R\library\ (packages du runtime R)

5) Dépannage
- Si le port 3838 est occupé: modifier PORT dans run_calculetteRWA.bat
- Si une librairie manque: exécuter tools/build_packages.R sur machine de build
