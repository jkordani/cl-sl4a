[![Quicklisp](http://quickdocs.org/badge/cl-sl4a.svg)](http://quickdocs.org/cl-sl4a/)

This package allows communication with an sl4a server
(See http://code.google.com/p/android-scripting/)

once loaded, use (target-device "ip" port) to setup the sl4a server ip and port, and then (target-connect)

once done, the demonstration functions should work.  try (test-getinput)!