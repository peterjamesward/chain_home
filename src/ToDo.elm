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
--DONE: Drop any targets moved beyond 100 miles range.
--DONE: Info bar on calculator to be below the display not overlaid.
--DONE: Created raids are appearing overlaid, not queued up, because of how timing works. Fix this.
--DONE: Operate mode to generate a sequence of incoming raids from various bearings & heights. (max 14 active!)
--DONE: Create the raids in a box 100m east of station.
--DONE: Highlight raid strength buttons according to tutorial.
--DONE: Clicking any raid type checkbox clears the config display!
--DONE: Operate mode to (default to) only use learnt raid types.
--DONE: Complete tutorial, click Receiver and raids are highlighted wrongly.
--TODO: Find nice way to constrain to 14 active aircraft (plus two ground rays). (Perhaps just cull the oldest.)
--DONE: Clean up navigation. Lose the Learn menu. Add About page (info, credits).
--TODO: Test on Android tablet !!
--TODO: Installable on iOS. -- Probably means getting to grips with Cordova/PhoneGap.
--TODO: Reset WebGL time occasionally as the noise visibly degrades, or fix otherwise.
--TODO: Summary page that shows actual incoming tracks with user plots overlaid. Assessment??
--TODO: Fit it on 800x600 screen?
--NOPE: Raspberry Pi with touch screen ??
--NOPE: Physical knobs -- UI app gets gonio setting from server app.
--NOPE: Improve that by using WebSockets -> JS -> Subscription -> Msg.
--NOPE: Physical knobs -- headless app reads from Arduino (10 bit encoder).
