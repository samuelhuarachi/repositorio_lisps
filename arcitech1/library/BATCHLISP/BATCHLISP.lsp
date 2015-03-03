;;;--- BatchLisp.lsp     [ For AutoCAD Release 2000 thru 2004 ]
;;;
;;;
;;;
;;;--- Program to create a script file to run a selected AutoLisp program
;;;    on all drawings selected.
;;;
;;;
;;;
;;;--- Copyright 2004-2006 by JefferyPSanders.com
;;;    All rights reserved.
;;;
;;;
;;;
;;;--- Please send all comments or suggestions to jps@jefferypsanders.com
;;;
;;;
;;;
;;;--- Notes:
;;;
;;;    1.  This program requires the SELECTFILES routine by JefferyPSanders.com
;;;
;;;
;;;    2.  The AutoLisp file selected must be defined to use the same name to 
;;;        run the program as it's file name.
;;;
;;;        Example:
;;;
;;;            MYPROG.lsp must be defined as C:MYPROG() not C:MYP()
;;;
;;;
;;;    3.  The autolisp program cannot accept parameters.
;;;
;;;
;;;
;;;
;;;--- Revisions:
;;;
;;;    6/22/06 - Revised the slashes in the autolisp file name to load correctly.  
;;;
;;;
;;;
;;;


(defun C:BATCHLISP()

  ;;;--- Set a flag for unsuccessful script creation
  (setq gogo nil)

  ;;;--- Set a flag for a change in the SDI system variable
  (setq oldSDI nil)  

  ;;;--- If the SELECTFILES routine is found...
  (if(load "C:\\arcitech1\\library\\BATCHLISP\\selectfiles.lsp")
    (progn

      ;;;--- Let the user select the drawing files
      (if(setq files(selectfiles (getvar "dwgprefix") "*.dwg" 0))
        (progn

          ;;;--- Let the user select the AutoLisp program
          (if(setq lispFile(getfiled "Select AutoLisp File" "" "LSP" 8))
            (progn

              ;;;--- Check the slashes in the file name to make sure they are correct
              (repeat 10(setq lispFile(vl-string-subst "/" "\\" lispFile)))

              ;;;--- Open the script file to write
              (if(setq fil(open "BATCHLISP.SCR" "w"))
                (progn

                  ;;;--- Save the SDI setting [Single Document Interface]
                  (setq oldSDI (getvar "sdi"))
                  
                  ;;;--- Set the SDI system variable to 1 [Single Document Mode]
                  (setvar "sdi" 1)
                 
                  ;;;--- Send the command to turn the file dialog switch off to the script file
                  (princ (strcat "(setvar " (chr 34) "filedia" (chr 34) " 0)" ) fil)

                  ;;;--- Cycle through each drawing file
                  (foreach a files

                    ;;;--- Send the open command to the script file
                    (princ 
                      (strcat 
                        "\n(if(> (getvar "(chr 34) "dbmod" (chr 34) ") 0)"
                        "(command " (chr 34) ".open" (chr 34)" "(chr 34)"Y"(chr 34)" "
                      )
                      fil
                    )
                    (prin1 a fil)
                    (princ  ")" fil)
                    (princ (strcat "(command " (chr 34) ".open" (chr 34) " ")fil)
                    (prin1 a fil)
                    (princ "))" fil)
                  

                    ;;;--- Send the LOAD command to the script file
                    (princ (strcat "\n(load " (chr 34)) fil)
                    (princ lispFile fil)
                    (princ (strcat (chr 34) ")" ) fil)
                    (princ "\n" fil)
                    (princ (vl-filename-base lispFile) fil)
 
                    ;;;--- Send the SAVE command to the script file
                    (princ "\n.QSAVE" fil)
                    (princ "\n" fil)

                    ;;;--- Reset the FILEDIA before opening the next drawing
                    (princ (strcat "\n(setvar " (chr 34) "filedia" (chr 34) " 0)" ) fil)
                  )
                  
                  ;;;--- Finish the script file
                  (princ "\n" fil)

                  ;;;--- If SDI [Single Document Interface] was changed...
                  (if oldSDI

                    ;;;--- Set it back to it's original state before ending
                    (princ (strcat "\n(setvar " (chr 34) "sdi" (chr 34) " " (itoa oldSDI)")\n") fil)
                  )


                  ;;;--- Close the script file
                  (close fil)

                  ;;;--- Set the flag for successful script creation
                  (setq gogo T)

                )
                (alert "Could not open BATCHLISP.SCR to write.\nCheck your rights to the default directory.\n\nProgram Aborting!")
              )           
            )
            (alert "No AutoLisp program selected.\n\nProgram Aborting!")
          )  
        )
        (alert "No files selected.\n\nProgram Aborting!")
      )
    )
    ;;;--- Else the SELECTFILES routine was not found.
    (alert "This program requires the SELECTFILES routine\ncreated by JefferyPSanders.com.\n\nProgram Aborting!")
  )

  ;;;--- If the script file was created successfully...
  (if gogo
    (progn

      ;;;--- Alert the user
      (alert "\nScript file created successfully!\n Press okay to start.")

      ;;;--- Start the script file
      (command "script" "BATCHLISP.scr")

    )
  )
  (princ)
)
     