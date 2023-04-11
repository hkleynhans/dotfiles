@echo off

call :copy_file "nvim\init.lua"
call :copy_file "nvim\lua\hkleynhans\init.lua"
call :copy_file "nvim\lua\hkleynhans\packer.lua"
call :copy_file "nvim\lua\hkleynhans\remap.lua"
call :copy_file "nvim\lua\hkleynhans\set.lua"

call :copy_file "nvim\after\plugin\cmake-tools.lua"
call :copy_file "nvim\after\plugin\lsp.lua"
call :copy_file "nvim\after\plugin\neogit.lua"
call :copy_file "nvim\after\plugin\other.lua"
call :copy_file "nvim\after\plugin\telescope.lua"
call :copy_file "nvim\after\plugin\treesitter.lua"

goto :eof

:copy_file
echo Copying %1
copy "%LOCALAPPDATA%\%1" "%1"
exit /b /0
