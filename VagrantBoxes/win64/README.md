# VCell Numerics build for Windows 64-bit using Mingw-w64
build configuration for VCell numerics

should be launched from a Windows host 
currently requires connection to UCHC internal subversion repository
requires Vagrant and Powershell 3.0 (or greater?)

vagrant up

vagrant powershell -c \vagrant\build.ps1

or on linux/mac use WinRM directly via pywinrm package

pip install pywinrm

'''text
>> import pywinrm
>> winrmsession = winrm.Session('192.168.50.4',auth=('vagrant','vagrant'))
>> result = winrmsession.run_ps("\vagrant\build.ps1")
>> result
'''

(also look into python-vagrant package for automation).
