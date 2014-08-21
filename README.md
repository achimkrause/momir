This is a script using a POS58 thermal printer to print random Magic: The Gathering cards according to the rules of "Momir Basic".


How to use
----------

Make sure you have the Glasgow Haskell Compiler (ghc) installed. First obtain the AllSets.json file from [link]mtgjson.com, place it in this folder, then run install.sh.

Afterwards, run main. You can repeatedly enter a number to print out a random creature card with the specified converted mana cost. Entering "q" will terminate the program.

The script expects the printer to be mounted at /dev/usb/lp0, the corresponding value can be changed in the file Printer.hs (pos58path).
If the file isn't found, the script will instead print card data to the console.

Make sure to set write access to that file (sudo a+w /dev/usb/lp0), this has to be done every time you connect the printer.

Upcoming:
---------

 * Flip- and split cards (currently, you only get the main side)

 * Mana symbols etc. (using the POS58 commands for user-defined fonts)
