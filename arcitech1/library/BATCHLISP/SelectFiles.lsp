;;;--- SELECTFILES.lsp      Version 1.0     3/25/04
;;;
;;;
;;;--- Allows multiple selection of any type of file. 
;;;
;;;    Returns a list of the file names including the path.
;;;
;;;
;;;
;;;--- Usage:
;;;
;;;    (selectfiles filePath Extension verifyDrives)
;;;
;;;    Parameters:
;;;
;;;      filePath     - Complete path as string.   Example: "C://ACAD"
;;;                     Use nil for current directory
;;;
;;;      Extension    - DOSsy filter as a string.             Example: "*.dwg"
;;;                     All wildcards accepted.
;;;
;;;      verifyDrives - Integer value of 1 will verify all drives 
;;;                     before adding them to the dialog box drop
;;;                     down list.
;;;                     Any other value will skip drive verification
;;;                     and all drives will show up in the list.
;;;
;;;                     No error will occur if the user selects a drive
;;;                     that is unavailable.
;;;
;;;                     IMPORTANT NOTE: Drive verification can be slow
;;;                     if you have a lot of drives with files and folders.
;;;
;;;
;;;--- Other notes of interest:
;;;
;;;    Selecting the "."  in the directory list box will take you back to the root.
;;;    Selecting the ".." in the directory list box will take you up a directory. 
;;;
;;;
;;;
;;;
;;;--- This program is offered as is without warranty.  
;;;    Feel free to modify,copy,destroy,sell,buy,rent,make fun of, tease or set fire to.
;;;    If you modify the program please remove the version number and my email address.
;;;
;;;    Please send comments or suggestions to jps@jefferypsanders.com
;;;
;;;
;;;
;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;--- Function to fill in the directories and files in the dialog box
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun UpdateDialog ()

  ;;;--- Clear the list in the dialog box
  (start_list "files")
  (end_list)

  ;;;--- Wipe out the previous current directory and
  ;;;    inform the user of a possible delay
  (set_tile "currentdirectory" "Working...")

  ;;;--- Get the directory names in a list
  (setq ListOfDirs (VL-Directory-Files FilePath nil -1))
  
  ;;;--- Get the file names in a list
  (setq ListOfFils (VL-Directory-Files FilePath Extension 1))

  ;;;--- Sort the directories
  (setq ListOfDirs (VL-Sort ListOfDirs (function (lambda (a b)(< (strcase a) (strcase b))))))

  ;;;--- Sort the file names
  (setq ListOfFils (VL-Sort ListOfFils (function (lambda (a b)(< (strcase a) (strcase b))))))

  ;;;--- Add the directories to the dialog list box
  (start_list "directories")
  (mapcar 'add_list ListOfDirs)
  (end_list)

  ;;;--- Add the file names to the dialog list box
  (start_list "files")
  (mapcar 'add_list ListOfFils)
  (end_list)

  ;;;--- Get the number of directories
  (setq dirCount(length ListOfDirs))

  ;;;--- Don't count the "." and the ".."
  (if(member "."  ListOfDirs)(setq dirCount(- dirCount 1)))
  (if(member ".." ListOfDirs)(setq dirCount(- dirCount 1)))  

  ;;;--- Count the number of directories and display it for the user
  (set_tile "DIRCOUNT" (strcat (itoa dirCount) " directories found."))

  ;;;--- Count the number of files and display it for the user
  (set_tile "FILCOUNT"(strcat (itoa (length ListOfFils))" files found." ))

  ;;;--- Update the current directory tile
  (set_tile "currentdirectory" (strcat "Current Directory: " FilePath))
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;--- Function to update the dialog box when a directory is clicked
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun UpdateDrives()

  ;;;--- Get the drive attribute
  (setq myDrive(get_tile "drives"))

  ;;;--- Find the selected directory
  (setq myDrive (nth (atoi myDrive) driveList))

  ;;;--- Reset the file path
  (setq FilePath (strcat myDrive "\\"))

  ;;;--- Reset the current directories tile
  (set_tile "currentdirectory" FilePath)
  
  ;;;--- Update the dialog box to show the new files and directories
  (UpdateDialog)
)






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;--- Function to update the dialog box when a directory is clicked
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun UpdateDirectory()

  ;;;--- Get the directories attribute
  (setq myPath(get_tile "directories"))

  ;;;--- Find the selected directory
  (setq myPath (nth (atoi myPath) ListOfDirs))

  ;;;--- Use a conditional loop to decide what to do next
  (cond
    
    ;;;--- If "." was selected, strip the path back to the drive
    ((and (= myPath ".")(> (strlen FilePath)3))

       ;;;--- Set the path to the first three characters
       (setq FilePath (substr FilePath 1 3))
   
    ) 
    
    ;;;--- If ".." was selected...
    ((and (= myPath "..")(> (strlen FilePath) 3))
      (progn

        ;;;--- Loop through the string from back to front stripping
	;;;    off one character at a time looking for a "\\".  When
	;;;    you find it, save the file path from the start of the
	;;;    string to the "\\" found.  This will give us a new
	;;;    path one directory up.

	;;;--- Get the index of the last character in the file path string
	(setq lChar(- (strlen FilePath) 1))

	;;;--- Strip off the last character
	(setq FilePath (substr FilePath 1 lChar))

	;;;--- While the last character is not a "\\" character
        (while (/= (substr FilePath lChar 1) "\\")

	  ;;;--- Move left one character to test it
          (setq lChar (- lChar 1))
	)

	;;;--- Save the new file path
        (setq FilePath (substr FilePath 1 lChar))
      )  
    )

    ;;;--- Else the user must have selected a directory on the same level
    ;;;    so simply add the directory to the path
    (T  (setq FilePath (strcat FilePath myPath "\\")))
  )

  ;;;--- Update the dialog box to show the new files and directories
  (UpdateDialog)
)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;--- Function to save the selections from the dialog box
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun saveVars()


  ;;;--- Set up an empty list to hold the selected file names
  (setq fileList(list))
  
  ;;;--- Get the files attribute
  (setq selIndexes(get_tile "files"))

  ;;;--- Set up a counter
  (setq cntr 1)

  ;;;--- Cycle through each item in the list
  (while (setq item (read selIndexes))

    ;;;--- Save the path and selected file name in the list
    (setq fileList (append fileList (list (strcat FilePath (nth item ListOfFils)))))

    ;;;--- Check for blank or empty selections
    (while
      (and
        (/= " " (substr selIndexes cntr 1))
        (/= ""  (substr selIndexes cntr 1))
      )
      (setq cntr (+ cntr 1))
    )

    ;;;--- Get the next item
    (setq selIndexes (substr selIndexes cntr))
  )

  ;;;--- Return the file list
  fileList
)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;--- Function to find all of the available drives
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun findDrives(dList)

  ;;;--- Set up an empty list to hold the verified drives
  (setq verifiedList(list))

  ;;;--- Cycle through every possible drive letter
  (foreach a dList

    ;;;--- Inform the user of the progress
    (princ "\n Verifying Drive ")(princ a)(princ " ... ")

    ;;;--- If this drive contains files or folders
    (if(vl-directory-files (strcat a "//") "*.*" 1)
       (progn

	 ;;;--- Add the drive to the verified drive list
         (setq verifiedList(append verifiedList (list a)))

	 ;;;--- Inform the user
	 (princ " - Verified!")
       )

       ;;;--- Inform the user of failure
       (princ " - Drive Unavailable.")
    )
  )

  ;;;--- Clear the command line
  (princ "\n.\n.")

  ;;;--- Inform the user of the drives verified
  (princ "\n Verified ")(princ (length verifiedList))(princ " Drives.")

  ;;;--- Return the list of verified files
  verifiedList
)  

 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;--- Main function to select multiple drawings
;;;
;;;    Parameters:
;;;
;;;      filePath     - Complete path as string.   Example: "C://ACAD"
;;;                     Use nil for current directory
;;;
;;;      Extension    - DOSsy filter as a string.             Example: "*.dwg"
;;;                     All wildcards accepted.
;;;
;;;      verifyDrives - Integer value of 1 will verify all drives 
;;;                     before adding them to the dialog box drop
;;;                     down list.
;;;                     Any other value will skip drive verification
;;;                     and all drives will show up in the list.
;;;
;;;                     No error will occur if the user selects a drive
;;;                     that is unavailable.
;;;
;;;                     IMPORTANT NOTE: Drive verification can be slow
;;;                     if you have a lot of drives with files and folders.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun SelectFiles(filePath Extension verifyDrives)

  ;;;--- Set the file path to the default
  (if(= filePath nil)(setq filePath (getvar "dwgprefix")))

  ;;;--- Build a drive list
  (setq driveList
    (list "A:" "B:" "C:" "D:" "E:" "F:" "G:" "H:" "I:" "J:" "K:" "L:" "M:"
	  "N:" "O:" "P:" "Q:" "R:" "S:" "T:" "U:" "V:" "W:" "X:" "Y:" "Z:"
  ) )

  ;;;--- Find all of the available drives if verifyDrives equals 1
  (if(= verifyDrives 1)
    (setq driveList(findDrives driveList))
  )

  ;;;--- Force the command line to update.
  (terpri)

  ;;;--- Find the current drive specified by the file path
  (setq currentDrive (strcase(substr filePath 1 2)))

  ;;;--- Set the drive list box to have the current drive selected
  (setq driveIndex(member currentDrive driveList))
  (setq driveIndex(- (length driveList) (length driveIndex)))
  (setq currentDrive(nth driveIndex driveList))

  ;;;--- Load the dialog box
  (setq dcl_id (load_dialog "C:\\arcitech1\\library\\BATCHLISP\\SELECTFILES.dcl"))
  
  ;;;--- See if it loaded
  (if (not (new_dialog "SELECTFILES" dcl_id) ) (exit))

  ;;;--- Add the drives to the dialog list box
  (start_list "drives")
  (mapcar 'add_list driveList)
  (end_list)

  ;;;--- Set the current drive to be selected
  (set_tile "drives" (itoa driveIndex))  

  ;;;--- Update the directory and file list boxes
  (UpdateDialog)

  ;;;--- Set up the action sequences from the dialog box
  (action_tile "drives" "(UpDateDrives)")
  (action_tile "directories" "(UpDateDirectory)")
  (action_tile "cancel" "(setq ddiag 1)(done_dialog)")
  (action_tile "accept" "(setq ddiag 2)(setq selectedFiles(saveVars))(done_dialog)") 

  ;;;--- Display the dialog box
  (start_dialog)

  ;;;--- If the CANCEL button was pressed, exit quietly
  (if (= ddiag 1)
    (setq SelectedFiles nil)
  )

  ;;;--- Return the list of files if they exist, else return nil
  SelectedFiles
)
