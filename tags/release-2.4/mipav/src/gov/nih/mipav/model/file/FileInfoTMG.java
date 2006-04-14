package gov.nih.mipav.model.file;

import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.model.structures.*;

/**
*   This structures contains the information that describes how
*   a TMG image is stored on disk.
*
*/

public class FileInfoTMG extends FileInfoBase {
    private String titleStr = null;
    private String nameStr = null;
    private String commentStr = null;
    private String timeStr = null;

    /**
    *  FileInfoTMG      - file info storage constructor
    *  @param name        file name
    *  @param directory   directory
    *  @param format      file format
    */
    public FileInfoTMG(String name, String directory, int format) {
        super(name, directory, format);
    }

    public void setTitleStr(String titleStr) {
      this.titleStr = titleStr;
    }

    public void setNameStr(String nameStr) {
      this.nameStr = nameStr;
    }

    public void setCommentStr(String commentStr) {
      this.commentStr = commentStr;
    }

    public void setTimeStr(String timeStr) {
      this.timeStr = timeStr;
    }


    /**
    *  Displays the file information
    *  @param dlog    dialog box that is written to
    *  @param matrix  transformation matrix
    */
    public void displayAboutInfo(JDialogBase dlog, TransMatrix matrix){
        JDialogText dialog = (JDialogText) dlog;
        displayPrimaryInfo(dialog, matrix);
        dialog.append("\n\n                Other information\n\n");
        if (titleStr != null) {
          dialog.append("Title = " + titleStr + "\n");
        }
        if (nameStr != null) {
          dialog.append("Name = " + nameStr + "\n");
        }
        if (commentStr != null) {
          dialog.append("Comment = " + commentStr + "\n");
        }
        if (timeStr != null) {
          dialog.append("Time = " + timeStr + "\n");
        }
    }
}
