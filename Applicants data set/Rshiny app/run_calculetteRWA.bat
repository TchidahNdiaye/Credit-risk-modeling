@echo off
setlocal EnableExtensions EnableDelayedExpansion

REM ==========================================
REM  calculetteRWA - 
REM ==========================================

set APP_HOME=%~dp0
set R_HOME=%APP_HOME%R
set APP_DIR=%APP_HOME%app

REM Local user dirs (avoid network write/locks)
set LOCAL_BASE=%LOCALAPPDATA%\calculetteRWA
if not exist "%LOCAL_BASE%" mkdir "%LOCAL_BASE%"
if not exist "%LOCAL_BASE%\tmp" mkdir "%LOCAL_BASE%\tmp"
if not exist "%LOCAL_BASE%\logs" mkdir "%LOCAL_BASE%\logs"

set TMPDIR=%LOCAL_BASE%\tmp
set TEMP=%LOCAL_BASE%\tmp
set TMP=%LOCAL_BASE%\tmp

set LOG_FILE=%LOCAL_BASE%\logs\startup.log

REM Prefer external library folder if exists, else fallback to R/library
set LIB_APP=%APP_HOME%library
set LIB_R=%APP_HOME%R\library

if exist "%LIB_APP%" (
  set R_LIBS_USER=%LIB_APP%
  set R_LIBS=%LIB_APP%
) else (
  set R_LIBS_USER=%LIB_R%
  set R_LIBS=%LIB_R%
)

REM Ensure project profile is used
set R_PROFILE_USER=%APP_DIR%\.Rprofile

REM Port (change if needed)
set PORT=3838

echo [%DATE% %TIME%] Starting calculetteRWA on http://127.0.0.1:%PORT% > "%LOG_FILE%"
echo APP_HOME=%APP_HOME% >> "%LOG_FILE%"
echo R_HOME=%R_HOME% >> "%LOG_FILE%"
echo R_LIBS_USER=%R_LIBS_USER% >> "%LOG_FILE%"
echo TMPDIR=%TMPDIR% >> "%LOG_FILE%"

REM Launch Shiny
"%R_HOME%\bin\R.exe" -e "options(shiny.port=%PORT%, shiny.host='127.0.0.1'); shiny::runApp('%APP_DIR%', launch.browser=TRUE)" >> "%LOG_FILE%" 2>&1

endlocal
