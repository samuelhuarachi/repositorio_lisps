SELECTFILES : dialog {  
      label="Select Files  -  Copyright 2004 by JefferyPSanders.com  All rights reserved.";
      : text {
        key = "currentdirectory";
        is_bold = true;
      } 
      : row { 
        :column {
          : popup_list {
            key = "drives";  
            label = "Select a Drive :";  
            width = 25;
            fixed_width_font = true;
          } 
          : list_box {
            key = "directories";  
            label = "Select a Directory :";  
            width = 25;
            fixed_width_font = true;
          } 
        }
        : list_box {
          key = "files"; 
          label = "Select Files :"; 
          width = 30; 
          multiple_select = true;
          fixed_width_font = true;
        } 
      } 
      : row { 
        : text {
          key = "DIRCOUNT";
        } 
        : text {
          key="FILCOUNT";
        } 
      } 
      : row {
        : button {
          key = "accept";
          label = "  Okay  ";
          is_default = true;
        }
        : button {
          key = "cancel";
          label = "Cancel";
          is_default = false;
          is_cancel = true;
        }
      }
} 
