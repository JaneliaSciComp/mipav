package gov.nih.mipav.view.renderer;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;
import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;

/**
 *  Dialog that gets input needed for saving the 3D rendering motions
 *	to an AVI file.  User can enter subsample and frame rate.
 *
 *		@version    0.1 Oct 2001
 *      @author  Neva Cherniavsky
 *      @see JDialogMouseRecorder
 */
public class JDialogRendererAVI
    extends JDialogBase {

  private JTextField rateBox;

  private double rate;
  private JCheckBox windowLevelCheckBox;
  private boolean applyWindowLevel;

  /**
   *	@param parent   	Parent frame.
   */
  public JDialogRendererAVI(Frame theParentFrame) {
    super(theParentFrame, true);
    init();
  }

  /**
   *	@param parent   	Parent frame.
   *   @param ui           User interface.
   */
  public JDialogRendererAVI(Frame theParentFrame, ViewUserInterface ui) {
    super(theParentFrame, true);
    init();
  }

  /**
   *	Used primarily for the script to store variables and run the algorithm.  No
   *	actual dialog will appear but the set up info and result image will be stored here.
   *	@param UI   The user interface, needed to create the image frame.
   */
  public JDialogRendererAVI(ViewUserInterface UI) {
    super();
  }

  /**
   *	Sets up the GUI (panels, buttons, etc) and displays it on the screen.
   */
  private void init() {
    setForeground(Color.black);
    setTitle("Set AVI options");

    JLabel rateLabel = new JLabel("Frame rate");
    rateLabel.setFont(MipavUtil.font12);
    rateLabel.setForeground(Color.black);

    rateBox = new JTextField("21", 3);

    JLabel rate2Label = new JLabel("frames per second");
    rate2Label.setFont(MipavUtil.font12);
    rate2Label.setForeground(Color.black);

    JPanel ratePanel = new JPanel();
    ratePanel.add(rateLabel);
    ratePanel.add(rateBox);
    ratePanel.add(rate2Label);

    windowLevelCheckBox = new JCheckBox("Apply window and level", true);
    JPanel windowLevelPanel = new JPanel();
    windowLevelPanel.add(windowLevelCheckBox);

    JPanel topPanel = new JPanel();
    topPanel.setLayout(new BoxLayout(topPanel, BoxLayout.Y_AXIS));
    topPanel.add(ratePanel);
    topPanel.add(windowLevelPanel);
    topPanel.setBorder(buildTitledBorder("AVI options"));

    JPanel buttonPanel = new JPanel();

    OKButton = buildOKButton();
    cancelButton = buildCancelButton();
    buttonPanel.add(OKButton);
    buttonPanel.add(cancelButton);

    JPanel mainPanel = new JPanel();
    mainPanel.setLayout(new BorderLayout());
    mainPanel.add(topPanel, BorderLayout.CENTER);
    mainPanel.add(buttonPanel, BorderLayout.SOUTH);

    getContentPane().add(mainPanel);
    pack();
    setVisible(true);
  }

  /**
   *	Accessor that sets the frame rate.
   *	@param rate		Frame rate to set.
   */
  public void setFrameRate(double rate) {
    this.rate = rate;
  }

  /**
   * Accessor that sets if the window and level slider settings
   *          are applied to the saved image
   * @param applyWindowLevel boolean
   */
  public void setApplyWindowLevel(boolean applyWindowLevel) {
      this.applyWindowLevel = applyWindowLevel;
  }

  /**
   *	Accessor that gets the frame rate.
   *	@return	The frame rate.
   */
  public double getFrameRate() {
    return rate;
  }

  /**
   * Accessor that gets the applyWindowLevel boolean
   * @return boolean
   */
  public boolean getApplyWindowLevel() {
      return applyWindowLevel;
  }

  /**
   *	Takes the following actions:
   *	<ul>
   *	<li>OK Button		- gets the frame rate from the text box and the subsample parameter
   *						from the combo boxes</li>
   *	<li>Cancel Button	- closes the dialog without doing anything</li>
   *	</ul>
   *	@param event  Event that triggers function.
   */
  public void actionPerformed(ActionEvent event) {
    String command = event.getActionCommand();

    if (command.equals("OK")) {
      if (testParameter(rateBox.getText(), 0, 100)) {
        rate = Double.valueOf(rateBox.getText()).doubleValue();
      }
      applyWindowLevel = windowLevelCheckBox.isSelected();
      dispose();
    }
    else if (command.equals("Cancel")) {
      cancelFlag = true;
      dispose();
    }
  }

}
