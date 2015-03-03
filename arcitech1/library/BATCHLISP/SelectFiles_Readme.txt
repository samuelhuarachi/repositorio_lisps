SELECTFILES.lsp ReadMe.txt file.           Version 1.0        3/25/04
---------------------------------------------------------------------------

 Allows multiple selection of any type of files. 

 Returns a list of the file names including the path.



 Usage:

    (selectfiles filePath Extension verifyDrives)

    Parameters:

      filePath     - Complete path as string.   Example: "C://ACAD"
                     Use nil for current directory

      Extension    - DOSsy filter as a string.  Example: "*.dwg"
                     All wildcards accepted.

      verifyDrives - Integer value of 1 will verify all drives 
                     before adding them to the dialog box drop
                     down list.
                     Any other value will skip drive verification
                     and all drives will show up in the list.
                     No error will occur if the user selects a drive
                     that is unavailable.

                     IMPORTANT NOTE: Drive verification can be slow
                     if you have a lot of drives with files and folders.


 Other notes of interest:

    Selecting the "."  in the directory list box will take you back to the root.
    Selecting the ".." in the directory list box will take you up a directory. 


 This program is offered as is without warranty.  
 Feel free to modify,copy,destroy,sell,buy,rent,make fun of, tease or set fire to.
 If you modify the program please remove the version number and my email address.

 Please send comments or suggestions to jps@jefferypsanders.com

