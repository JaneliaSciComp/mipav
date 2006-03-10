package gov.nih.mipav.view;


import java.awt.event.*;
import java.awt.*;
import javax.swing.*;


/**
 *       This is a color chooser designed so that the preview panel is
 *       not shown.
 *
 *		@version    0.1 Jun 1, 1999
 *		@author     Neva Cherniavsky
 *
 */

public class ViewJColorChooser
    extends JColorChooser {

  private JDialog colorDialog;

  /**
   *  Constructor   Constructs the color chooser panel and dialog.
   *  @param parent      the array of x coordinates to be plotted in the graph
   *  @param title       the title of the frame
   *  @param OKListener  class that will respond to the user pressing OK
       *  @param CancelListener  class that will respond to the user pressing Cancel
   */
  public ViewJColorChooser(Frame parent, String title,
                           ActionListener OKListener,
                           ActionListener CancelListener) {
    super();

    colorDialog = JColorChooser.createDialog(parent,
                                             title,
                                             false,
                                             this,
                                             OKListener,
                                             CancelListener);
    this.setPreviewPanel(new JPanel());
    colorDialog.setSize(450, 310);
    colorDialog.setResizable(false);
    colorDialog.setVisible(true);
  }

  public boolean isDialogVisible() {
    if (colorDialog == null)
      return false;
    else
      return colorDialog.isVisible();
  }

  public void setDialogVisible(boolean vis) {
    if (colorDialog != null) {
      colorDialog.setVisible(vis);
    }
  }

}
