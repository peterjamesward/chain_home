
Make an external goniometer (and probably a range knob too).

Current thinking is that this will use a Bourns EMS22 position sensor that connects via SSI
to the GPIO port on a Raspberry Pi (via a level shifter if 5V).

It is possible to run Elm on a Pi, easiest using a Docker container. 

Elm cannot of course read SSI, so here has to be some other process, which could be Node, Python,
anything really to read the goniometer position. Has to get into Elm somehow, for which the key
options are JS ports, or HTTP.

Taking this further, the goniometer could be considered as a network device, self-contained
with its own HTTP server, so we can easily read from Elm with HTTP and we have a nice clean 
separation.

Might still be wise to do a POC with the Pi, but could end up (and would be nice btw) with
a micro-controller. Looking around, Arduino would suit but I suspect it would be easier to
go down the Raspberry Pi Pico W route. This can be programmed in MicroPython or CircuitPython
and has WiFi and Bluetooth built-in. Setting up a simple web server seems easy, and libraries
exist to help with reading SSI.

https://github.com/todbot/pico8enc
https://circuitpython.org
https://core-electronics.com.au/guides/getting-started-with-rotary-encoders-examples-with-raspberry-pi-pico/
etc.
https://www.robertthasjohn.com/post/how-to-set-up-the-raspberry-pi-pico-for-development-on-macos

But that would only give a goniometer. We will need also range knob, sundry switches and lights
and (of course) the "tube". Which to do at reasonable cost implies using a proper Pi anyway, so
why bother with the Pico?