package gov.nih.mipav.view.dialogs;


import java.awt.*;
import java.awt.event.*;
import java.text.*;

import javax.swing.*;


/**
 * Confirmation Dialog giving user the choice to choose which circles to draw.
 *
 * @author   not attributable
 * @version  1.0
 */
public class JDialogHoughCircleChoice extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 0;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    private double x0Array[];
    
    private int xDimSource;
    
    private double y0Array[];
    
    private int yDimSource;
    
    private double radArray[];
    
    private double maxRad;
    
    private int countArray[];
    
    private boolean selectedCircle[];
    
    private int numCirclesFound;
    
    private JCheckBox selectedArray[];

    /** DOCUMENT ME! */
    private boolean okayPressed = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new JDialogHoughCircleChoice object.
     *
     * @param  theParentFrame  DOCUMENT ME!
     * @param  x0Array
     * @param  xDimSource
     * @param  y0Array
     * @param  yDimSource
     * @param  radArray
     * @param  maxRad
     * @param  countArray
     * @param  selectedCircle
     */
    public JDialogHoughCircleChoice(Frame theParentFrame, double x0Array[], int xDimSource, double y0Array[], int yDimSource,
                                  double radArray[], double maxRad, int countArray[],
                                  boolean selectedCircle[]) {
        super(theParentFrame, true);
        this.x0Array = x0Array;
        this.xDimSource = xDimSource;
        this.y0Array = y0Array;
        this.yDimSource = yDimSource;
        this.radArray = radArray;
        this.maxRad = maxRad;
        this.countArray = countArray;
        this.selectedCircle = selectedCircle;
        numCirclesFound = selectedCircle.length;
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Checks to see if the OK or Cancel buttons were pressed.
     *
     * @param  event  Event that triggered this function.
     */
    public void actionPerformed(ActionEvent event) {
        int i;

        if (event.getSource() == OKButton) {
            for (i = 0; i < numCirclesFound; i++) {
                if (selectedArray[i].isSelected()) {
                    selectedCircle[i] = true;
                } // if (selectedArray[i].isSelected())
                else {
                    selectedCircle[i] = false;
                }
            } // for (i = 0; i < numCirclesFound; i++)
            okayPressed = true;
        } else {
            super.actionPerformed(event);
        }

        selectedArray = null;
        dispose();
    }

    /**
     * Check to see if line was selected or deselected
     *
     * @param  event  (combo box event)
     */
    public void itemStateChanged(ItemEvent event) {
        
    }

    /**
     * Was the okay button pressed.
     *
     * @return  boolean was okay pressed
     */
    public boolean okayPressed() {
        return okayPressed;
    }

    /**
     * Creates and displays dialog.
     */
    private void init() {
        DecimalFormat df = new DecimalFormat("0.#");
        int i;
        JLabel x0LabelArray[] = new JLabel[numCirclesFound];
        JLabel y0LabelArray[] = new JLabel[numCirclesFound];
        JLabel radLabelArray[] = new JLabel[numCirclesFound];
        JLabel countLabelArray[] = new JLabel[numCirclesFound];
        selectedArray = new JCheckBox[numCirclesFound];
        setTitle("Hough transform circle selection");
  
        JPanel createPanel = new JPanel(new GridBagLayout());
        createPanel.setBorder(buildTitledBorder("Select circles for display"));
        
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(0, 20, 0, 0);
        
        JLabel x0Label = new JLabel("x0(0-"+ String.valueOf(xDimSource-1) + ")");
        x0Label.setFont(serif12);
        createPanel.add(x0Label, gbc);
        
        gbc.gridx = 1;
        JLabel y0Label = new JLabel("y0(0-" + String.valueOf(yDimSource-1) + ")");
        y0Label.setFont(serif12);
        createPanel.add(y0Label, gbc);
        
        gbc.gridx = 2;
        JLabel radLabel = new JLabel("radius(0 - " + String.valueOf(maxRad) + ")");
        radLabel.setFont(serif12);
        createPanel.add(radLabel, gbc);
        
        gbc.gridx = 3;
        JLabel countLabel = new JLabel("Points counted");
        countLabel.setFont(serif12);
        createPanel.add(countLabel, gbc);
        
        gbc.gridx = 4;
        JLabel selectLabel = new JLabel("Select circles");
        selectLabel.setFont(serif12);
        createPanel.add(selectLabel, gbc);
        
        for (i = 0; i < numCirclesFound; i++) {
            gbc.gridx = 0;
            gbc.gridy++;
            x0LabelArray[i] = new JLabel(df.format(x0Array[i]));
            x0LabelArray[i].setFont(serif12);
            createPanel.add(x0LabelArray[i], gbc);
            
            gbc.gridx = 1;
            y0LabelArray[i] = new JLabel(df.format(y0Array[i]));
            y0LabelArray[i].setFont(serif12);
            createPanel.add(y0LabelArray[i], gbc);
            
            gbc.gridx = 2;
            radLabelArray[i] = new JLabel(df.format(radArray[i]));
            radLabelArray[i].setFont(serif12);
            createPanel.add(radLabelArray[i], gbc);
            
            gbc.gridx = 3;
            countLabelArray[i] = new JLabel(String.valueOf(countArray[i]));
            countLabelArray[i].setFont(serif12);
            createPanel.add(countLabelArray[i], gbc);
            
            gbc.gridx = 4;
            selectedArray[i] = new JCheckBox("");
            selectedArray[i].setSelected(false);
            selectedArray[i].setFont(serif12);
            createPanel.add(selectedArray[i], gbc);
            
        } // for (i = 0; i < numCirclesFound; i++)

        
        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);

        mainDialogPanel.add(createPanel);
        mainDialogPanel.add(buttonPanel, BorderLayout.SOUTH);

        getContentPane().add(mainDialogPanel);

        pack();
        setVisible(true);
    }
}
