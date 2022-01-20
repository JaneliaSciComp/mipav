package gov.nih.mipav.view.dialogs;


import java.awt.*;
import java.awt.event.*;
import java.text.*;

import javax.swing.*;


/**
 * Confirmation Dialog giving user the choice to choose which cardioids to draw.
 *
 * @author   not attributable
 * @version  1.0
 */
public class JDialogHoughCardioidChoice extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 0;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    private double x0Array[];
    
    private int xDim;
    
    private double y0Array[];
    
    private int yDim;
    
    private double a0Array[];
    
    private double maxA;
    
    private double theta0Array[];
    
    private int countArray[];
    
    private boolean selectedCardioid[];
    
    private int numCardioidsFound;
    
    private JCheckBox selectedArray[];

    /** DOCUMENT ME! */
    private boolean okayPressed = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new JDialogHoughCardioidChoice object.
     *
     * @param  theParentFrame  DOCUMENT ME!
     * @param  x0Array
     * @param  xDim
     * @param  y0Array
     * @param  yDim
     * @param  a0Array
     * @param  maxA
     * @param  theta0Array
     * @param  countArray
     * @param  selectedCardioid
     */
    public JDialogHoughCardioidChoice(Frame theParentFrame, double x0Array[], int xDim, double y0Array[], int yDim,
                                  double a0Array[], double maxA, double theta0Array[], int countArray[],
                                  boolean selectedCardioid[]) {
        super(theParentFrame, true);
        this.x0Array = x0Array;
        this.xDim = xDim;
        this.y0Array = y0Array;
        this.yDim = yDim;
        this.a0Array = a0Array;
        this.maxA = maxA;
        this.theta0Array = theta0Array;
        this.countArray = countArray;
        this.selectedCardioid = selectedCardioid;
        numCardioidsFound = selectedCardioid.length;
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
            for (i = 0; i < numCardioidsFound; i++) {
                if (selectedArray[i].isSelected()) {
                    selectedCardioid[i] = true;
                } // if (selectedArray[i].isSelected())
                else {
                    selectedCardioid[i] = false;
                }
            } // for (i = 0; i < numCardioidsFound; i++)
            okayPressed = true;
        } else {
            super.actionPerformed(event);
        }

        selectedArray = null;
        dispose();
    }

    /**
     * Check to see if cardioid was selected or deselected
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
        double theta0;;
        JLabel x0LabelArray[] = new JLabel[numCardioidsFound];
        JLabel y0LabelArray[] = new JLabel[numCardioidsFound];
        JLabel a0LabelArray[] = new JLabel[numCardioidsFound];
        JLabel theta0LabelArray[] = new JLabel[numCardioidsFound];
        JLabel countLabelArray[] = new JLabel[numCardioidsFound];
        selectedArray = new JCheckBox[numCardioidsFound];
        setTitle("Hough transform cardioid selection");
  
        JPanel createPanel = new JPanel(new GridBagLayout());
        createPanel.setBorder(buildTitledBorder("Select cardioids for display"));
        
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(0, 20, 0, 0);
        
        gbc.gridx = 0;
        JLabel x0Label = new JLabel("x(0-"+ String.valueOf(xDim-1) + ")");
        x0Label.setFont(serif12);
        createPanel.add(x0Label, gbc);
        
        gbc.gridx = 1;
        JLabel y0Label = new JLabel("y(0-" + String.valueOf(yDim-1) + ")");
        y0Label.setFont(serif12);
        createPanel.add(y0Label, gbc);
        
        gbc.gridx = 2;
        JLabel radLabel = new JLabel("a(0 - " + String.valueOf(maxA) + ")");
        radLabel.setFont(serif12);
        createPanel.add(radLabel, gbc);
        
        gbc.gridx = 3;
        JLabel theta0Label = new JLabel("theta0(0 - 359.00)");
        theta0Label.setFont(serif12);
        createPanel.add(theta0Label, gbc);
        
        gbc.gridx = 4;
        JLabel countLabel = new JLabel("Points counted");
        countLabel.setFont(serif12);
        createPanel.add(countLabel, gbc);
        
        gbc.gridx = 5;
        JLabel selectLabel = new JLabel("Select cardioids");
        selectLabel.setFont(serif12);
        createPanel.add(selectLabel, gbc);
        
        for (i = 0; i < numCardioidsFound; i++) {
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
            a0LabelArray[i] = new JLabel(df.format(a0Array[i]));
            a0LabelArray[i].setFont(serif12);
            createPanel.add(a0LabelArray[i], gbc);
            
            gbc.gridx = 3;
            theta0 = theta0Array[i];
            if (theta0 < 0.0) {
            	theta0 = theta0 + 2.0 * Math.PI;
            }
            theta0 = 180.0 * theta0/Math.PI;
            theta0LabelArray[i] = new JLabel(df.format(theta0));
            theta0LabelArray[i].setFont(serif12);
            createPanel.add(theta0LabelArray[i], gbc);
            
            gbc.gridx = 4;
            countLabelArray[i] = new JLabel(String.valueOf(countArray[i]));
            countLabelArray[i].setFont(serif12);
            createPanel.add(countLabelArray[i], gbc);
            
            gbc.gridx = 5;
            selectedArray[i] = new JCheckBox("");
            selectedArray[i].setSelected(false);
            selectedArray[i].setFont(serif12);
            createPanel.add(selectedArray[i], gbc);
            
        } // for (i = 0; i < numCardioidsFound; i++)

        
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

