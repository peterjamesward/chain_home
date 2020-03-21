--DONE: Noise spike at about +0.35 in clip space (only on iPad).
--DONE: CRT block does not scale. Is this underlying layout issues on iOS? (No, it's not the WebGL. What it is?
--DONE: Reduce sorting burden - if that what's slowing the iPad. (It seems to have an impact.)
--DONE: (pass in number of raids, select sort size)
--DONE: Noise parameters to configuration.
--DONE: Treat formations as units, with their intrinsic dancing patterns.
--DONE: IFF.
--DONE: Electronic calculator output.
--DONE: Start raids further out.
--DONE: Go! button resets timer, restarts raids from origin.
--DONE: Stop text selection.
--DROP: Sensing is on the receiver, not the transmitter. (Actually, both. No changes required.)
--DONE: Forward & Back buttons for tutorial.
--DONE: Explanations on Calculator page.
--DONE: Tweak epsilon to stop endless range and bearing seeky behaviour.
--DONE: Move trace to lower on the CRT.
--DONE: Physical knobs -- get a headless little server app running with a Gonio API.
--DONE: Narrative and on-screen prompts.
--DONE: Each tutorial stage needs entry and exit (Model -> Model) to ensure consistency.
--DONE: PO to decide which raid scenarios to be in narrative.
--DROP: Range indicator should be front of the scale. Sure it used to be,
--DONE: Can we do a walk-through of a raid? -- Yes, WIP.
--DONE: Tutorial strings can be dynamic if we pass model in.
--DONE: Start tutorial with some general gonio swinging.
--DONE: Drop down learn menu selection should reset active tutorial.
--DONE: Clean up the Maybe madness in tutorial world.
--DONE: Select tutorial whilst active does not reset properly.
--DONE: Possibly, explain button in calc display not working. -- BECAUSE lookup now expects a tutorial!
--DONE: Learning menu of raid types (one, two, 3-6, IFF).
--DONE: Two planes same bearing scenario
--DONE: Two planes different bearings
--DONE: 3-6 scenario
--DONE: IFF scenario
--DONE: Info bar on calculator to be below the display not overlaid.
--TODO: Operate mode to generate a sequence of incoming raids from various bearings & heights. (max 14 active!)
--TODO: Operate mode to (default to) only use learnt raid types.
--DONE: Drop any targets moved beyond 100 miles range.
--TODO: Test on Android tablet !!
--TODO: Installable on iOS. -- Probbly means getting to grips with Cordova/PhoneGap.
--NOPE: Raspberry Pi with touch screen ??
--NOPE: Physical knobs -- UI app gets gonio setting from server app.
--NOPE: Improve that by using WebSockets -> JS -> Subscription -> Msg.
--NOPE: Physical knobs -- headless app reads from Arduino (10 bit encoder).
