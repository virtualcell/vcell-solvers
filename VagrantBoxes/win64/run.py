import winrm
winrmsession = winrm.Session('192.168.50.4',auth=('vagrant','vagrant'))
result = winrmsession.run_ps("\vagrant\build.ps1")
result
