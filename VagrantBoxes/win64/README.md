# VCell Numerics build for Windows 64-bit using Mingw-w64
build configuration for VCell numerics

should be launched from a Windows host 
requires Vagrant and Powershell 3.0 (or greater)

## on windows:
vagrant up
vagrant powershell -c \vagrant\build.ps1
vagrant halt


## on mac or linux using VirtualBox GUI, open powershell window:
vagrant up
... from GUI from VirtualBox or Remote Desktop
open Powershell window
PS> c:\vagrant\build.ps1
vagrant halt


## on windows or mac using pywinrm package for WinRM support (NOT YET WORKING):
### setup:
pip install pywinrm


### running:
vagrant up
python run.py
vagrant halt
