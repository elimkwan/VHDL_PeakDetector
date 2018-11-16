We have also implemented the LED funcionality to show the states we are at when we are doing debugging:
dataProc takes the left four LEDs (7 downto 4)
cmdProc takes the right four LEDs (3 downto 0)

If one would like to see how it works, make the following changes to these files:

For test.ucf:
  Find lines with "####" and remove the commented code

For peak_detector.vhd, cmdProc.vhd, dataProc.vhd:
  Find line with "---" and remove the commented code
  (Note: remeber to select match word when doing find & replace)

Then the LEDs should light up
Thanks