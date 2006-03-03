package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.view.*;
import java.awt.event.*;
import java.awt.*;

import javax.swing.*;

/**
 * <p>Title: </p>
 * <p>Description: </p>
 * <p>Copyright: Copyright (c) 2003</p>
 * <p>Company: </p>
 * @author not attributable
 * @version 1.0
 */

public class JDialogIntensityThreshold
    extends JDialogBase {

  private ViewUserInterface ui;
  private boolean doAverage;
  private JTextField threshField = null;

  private ViewJComponentEditImage component;

  public JDialogIntensityThreshold(JFrame theParentFrame,
                                   ViewUserInterface userInterface,
                                   ViewJComponentEditImage compImage,
                                   boolean average) {
    super(theParentFrame, false);
    ui = userInterface;
    this.component = compImage;
    doAverage = average;
    init();
    setVisible(true);
  }

  public void actionPerformed(ActionEvent e) {
    /**@todo Implement this java.awt.event.ActionListener abstract method*/
    Object source = e.getSource();
    if ( source == OKButton ) {
      float threshold = 0.0f;
      try {
        threshold = new Float(threshField.getText()).floatValue();
      }
      catch (Exception ex) {
        MipavUtil.displayError("Threshold must be a float value");
        return;
      }
      setVisible(false);
      component.graph25VOI_CalcInten(!doAverage, true, threshold);
      this.dispose();
    }
    else if ( source == cancelButton ) {
      setVisible(false);
      this.dispose();
    }
  }

  private void init() {
    setForeground(Color.black);
    if (doAverage) {
      setTitle("2.5D Average Intensity");
    }
    else {
      setTitle("2.5D Total Intensity");
    }

    JPanel mainPanel = new JPanel();
    mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.X_AXIS));
    mainPanel.setForeground(Color.black);

    mainPanel.setBorder(buildTitledBorder(""));

    JLabel threshLabel = new JLabel("Intensity threshold   ");
    threshLabel.setForeground(Color.black);
    threshLabel.setFont(serif12);

    threshField = new JTextField(4);

    mainPanel.add(threshLabel);
    mainPanel.add(threshField);

    JPanel buttonPanel = new JPanel();
    buildOKButton();
    buttonPanel.add(OKButton);
    buildCancelButton();
    buttonPanel.add(cancelButton);

    getContentPane().add(mainPanel, BorderLayout.NORTH);
    getContentPane().add(buttonPanel, BorderLayout.SOUTH);
    pack();
  }

}
