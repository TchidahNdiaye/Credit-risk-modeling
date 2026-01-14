@echo off
taskkill /IM R.exe /F >nul 2>&1
taskkill /IM Rterm.exe /F >nul 2>&1
echo Done.
