package gov.nih.mipav.view.renderer.surfaceview.rfaview;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.*;
import gov.nih.mipav.view.dialogs.JDialogBase;
import javax.swing.*;
import java.awt.event.*;
import java.awt.*;
import javax.swing.border.*;

public class JDialogBurnParameter extends JDialogBase {

  public float volume;
  public float radius;
  public float opacity;
  public float diameter;
  public float voltage;
  public float time;

  private JTextField diameterField;
  private JTextField timeField;
  private JTextField voltageField;
  public boolean ok = false;

  private JPanelProbe probePanel;

  public JDialogBurnParameter( JPanelProbe _probePanel, ViewJFrameBase _parentFrame) {
    super(_parentFrame, false);
    init();
    probePanel = _probePanel;
  }

  public void actionPerformed( ActionEvent event ) {
       String command = event.getActionCommand();

       if ( command.equals( "OK" ) ) {
           setVariables();
           setVisible(false);
           probePanel.startBurn();
       } else if ( command.equals( "Cancel" ) ) {
           setVisible(false);
       }
   }

   public void setVariables() {
      String timeString = timeField.getText();
      String diameterSting = diameterField.getText();
      diameter = Float.valueOf(diameterSting).floatValue();
      time = Float.valueOf(timeString).floatValue();
      ok = true;
   }


   private void init() {

      setTitle("Parameter");
      setLocation(300, 400);

      Box contentBox = new Box(BoxLayout.Y_AXIS);
      JPanel optionPanel = new JPanel();
      // make border
      optionPanel.setBorder(buildTitledBorder("Burn Parameter"));
      contentBox.add(optionPanel);
      // set layout
      GridBagLayout gbl = new GridBagLayout();
      GridBagConstraints gbc = new GridBagConstraints();
      optionPanel.setLayout(gbl);

      gbc.anchor = GridBagConstraints.NORTHWEST;

      // diameter
      optionPanel.add(Box.createHorizontalStrut(10));
      JLabel diameterLabel = new JLabel( "Burn diameter: " );
      diameterLabel.setFont( serif12 );
      diameterLabel.setForeground( Color.black );

      gbc.gridwidth = 2;
      gbl.setConstraints(diameterLabel, gbc);
      optionPanel.add(diameterLabel);
      optionPanel.add(Box.createHorizontalStrut(10));

      diameterField = new JTextField( 10 );
      diameterField.setText("0.1");
      MipavUtil.makeNumericsOnly(diameterField, true, true);

      gbc.gridwidth = GridBagConstraints.REMAINDER;
      gbl.setConstraints(diameterField, gbc);
      optionPanel.add(diameterField);


      // time

      optionPanel.add(Box.createHorizontalStrut(10));
      JLabel timeLabel = new JLabel(" Burn Time (s): ");
      timeLabel.setFont( serif12 );
      timeLabel.setForeground( Color.black );
      timeLabel.setRequestFocusEnabled(false);
      gbc.gridwidth = 2;
      gbl.setConstraints(timeLabel, gbc);
      optionPanel.add(timeLabel);
      optionPanel.add(Box.createHorizontalStrut(10));

      timeField = new JTextField( 10 );
      timeField.setText("1");

      MipavUtil.makeNumericsOnly(timeField, true, true);

      gbc.gridwidth = GridBagConstraints.REMAINDER;
      gbl.setConstraints(timeField, gbc);
      optionPanel.add(timeField);

      // voltage
      optionPanel.add(Box.createHorizontalStrut(10));
      JLabel voltageLabel = new JLabel(" Burn Voltage (v): ");
      voltageLabel.setFont(serif12);
      voltageLabel.setForeground(Color.black);

      gbc.gridwidth = 2;
      gbl.setConstraints(voltageLabel, gbc);
      optionPanel.add(voltageLabel);
      optionPanel.add(Box.createHorizontalStrut(10));
       voltageField = new JTextField(10);
      MipavUtil.makeNumericsOnly(voltageField, true, true);
      gbc.gridwidth = GridBagConstraints.REMAINDER;
      gbl.setConstraints(voltageField, gbc);
      optionPanel.add(voltageField);
      voltageLabel.setEnabled(false);
      voltageField.setEnabled(false);


      JPanel buttonPanel = new JPanel();
      OKButton = buildOKButton();
      cancelButton = buildCancelButton();
      buttonPanel.add( OKButton );
      buttonPanel.add( cancelButton );

      contentBox.add( buttonPanel );
      getContentPane().add( contentBox );
      pack();
      setVisible(true);
   }

}
