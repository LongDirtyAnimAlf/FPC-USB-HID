# FPC-USB-HID
usbcontroller.pas is a simple clone of the Jedi JVCL library to access Hid devices on Linux with FPC.
Very low level. No external libraries needed.
Not all functions fom the original JVCL library are available.
But this simple clone will allow you to control most HID devices.
It is designed to control Microchip MCUs with USB and the HID firmware.
But other applications can be made to function.

Also added a ported version of the original JvHidControllerClass.pas.
This can now also be used with FPC on Windows ! 

For Linux:
Please remember to apply the udev-rules to gain access to usb devices !!
Adapt the provided udev rule to your needs.

Simple demo included.