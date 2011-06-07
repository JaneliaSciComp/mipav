package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;
import javax.swing.event.*;





/**
 * Dialog to get the row and column numbers of checkerboard squares
 *
 * @see  ViewJComponentEditImage
 */
public class JDialogCheckerBoard extends JDialogBase implements ChangeListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 4180573157937289440L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JButton closeButton;

    /** DOCUMENT ME! */
    private ViewJComponentEditImage compImage;

    /** DOCUMENT ME! */
    private JCheckBox doCheckbox;

    /** DOCUMENT ME! */
    private boolean doReg = false;

    /** DOCUMENT ME! */
    private JLabel labelColumnNumber, labelRowNumber, speedLabel;

    /** DOCUMENT ME! */
    private Hashtable<Integer,JLabel> labelTable, labelTable2, speedLabelTable;

    /** DOCUMENT ME! */
    private int maxColumn;

    /** DOCUMENT ME! */
    private int maxRow;

    /** DOCUMENT ME! */
    private ViewJComponentRegistration regImage;

    /** DOCUMENT ME! */
    private JSlider slider, slider2, speedSlider;
    
    private JButton animateButton;

    /** DOCUMENT ME! */
    private JTextField textRowNumber, textColumnNumber;
    
    private boolean stopAnimate = false;
    
    public Thread animateThread;
    
    private int cc = 0;
    
    private int[] pixBufferB;
    
    private int[] cleanImageBufferB;
    
    private int rowNumber, columnNumber;
    
    private int ySep;
    
    private int[] maxExtents;
    
    private boolean isStopped = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog and sets up GUI components.
     *
     * @param  theParentFrame  Parent frame.
     * @param  compImg         Source image.
     */
    public JDialogCheckerBoard(Frame theParentFrame, ViewJComponentEditImage compImg) {
        super(theParentFrame, false);
        compImage = compImg;
        maxRow = Math.min(compImage.getImageA().getExtents()[1] / 4, 50);
        maxColumn = Math.min(compImage.getImageA().getExtents()[0] / 4, 50);
        setDefaultCloseOperation(DISPOSE_ON_CLOSE);
        setup();
    }

    /**
     * Creates new dialog and sets up GUI components.
     *
     * @param  theParentFrame  Parent frame.
     * @param  regImg          Source image.
     */
    public JDialogCheckerBoard(Frame theParentFrame, ViewJComponentRegistration regImg) {
        super(theParentFrame, false);
        regImage = regImg;
        maxRow = Math.min(regImage.getImageA().getExtents()[1] / 4, 50);
        maxColumn = Math.min(regImage.getImageA().getExtents()[0] / 4, 50);
        doReg = true;
        setDefaultCloseOperation(DISPOSE_ON_CLOSE);
        setup();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Sets parameters in ViewJComponentEditImage when Apply is pressed. Closes dialog box in response to both Apply and
     * Cancel buttons.
     *
     * @param  event  Event that triggers function.
     */
    @SuppressWarnings("unchecked")
    public void actionPerformed(ActionEvent event) {
        

        String command = event.getActionCommand();
        Object source = event.getSource();

        if ((command.equals("OK")) || (command.equals("Close"))) {

            if (doCheckbox.isSelected()) {
                rowNumber = slider.getValue();
                columnNumber = slider2.getValue();
            } else { // no checkerboarding
                rowNumber = -1;
                columnNumber = -1;
                //stopAnimate = true;
                setStopAnimate(true);
            }

            if (doReg) {
                regImage.setCheckerboard(rowNumber, columnNumber);
                regImage.repaint();
            } else {
            	//compImage.setCheckerboardCounter(0);
            	cc = 0;
            	
                compImage.setCheckerboard(rowNumber, columnNumber);
                compImage.setMakingCheckerboard(true);
                compImage.paintComponent(compImage.getGraphics());
                compImage.setMakingCheckerboard(false);

                if(compImage.isCheckerboarded()) {
                	if(rowNumber == 1 || columnNumber ==1) {
                		speedLabel.setEnabled(true);
                		speedSlider.setEnabled(true);
                		animateButton.setEnabled(true);
                	}else {
                		speedLabel.setEnabled(false);
                		speedSlider.setEnabled(false);
                		animateButton.setEnabled(false);
                	}
                }
            }
            
            

            if (command.equals("Close")) {

                if (doReg) {
                    regImage.checkerDialog = null;
                } else {
                    compImage.checkerDialog = null;
                }

                dispose();
            }
        } else if (command.equals("Cancel")) {

            if (doReg == true) {
                regImage.checkerDialog = null;
            } else {
                compImage.checkerDialog = null;
            }

            dispose();
        } else if (source == doCheckbox) {

            if (doCheckbox.isSelected()) {
                slider.setEnabled(true);
                slider2.setEnabled(true);
                labelRowNumber.setEnabled(true);
                labelColumnNumber.setEnabled(true);

                for (Enumeration<JLabel> en = slider.getLabelTable().elements(); en.hasMoreElements();) {
                    (en.nextElement()).setEnabled(true);
                }

                for (Enumeration<JLabel> en = slider2.getLabelTable().elements(); en.hasMoreElements();) {
                    (en.nextElement()).setEnabled(true);
                }
            } else {
                slider.setEnabled(false);
                slider2.setEnabled(false);
                labelRowNumber.setEnabled(false);
                labelColumnNumber.setEnabled(false);

                for (Enumeration<JLabel> en = slider.getLabelTable().elements(); en.hasMoreElements();) {
                    (en.nextElement()).setEnabled(false);
                }

                for (Enumeration<JLabel> en = slider2.getLabelTable().elements(); en.hasMoreElements();) {
                    (en.nextElement()).setEnabled(false);
                }
            }
        }else if (command.equals("animate")) {
        	if(animateButton.getText().equals("Start")) {

        		
        		setStopAnimate(false);
        		animateButton.setText("Stop");
        		compImage.setCheckerboardAnimate(true);
        		OKButton.setEnabled(false);
        		closeButton.setEnabled(false);
        		doCheckbox.setEnabled(false);
        		slider.setEnabled(false);
        		slider2.setEnabled(false);
        		speedSlider.setEnabled(false);
        		compImage.removeMouseListener(compImage);
        		compImage.removeMouseMotionListener(compImage);

        		animateThread = new Animate();
    	    	try {
    	    		animateThread.start();
    	    	}catch (Exception e) {
    				e.printStackTrace();
    				return;
    			}
        		
        		
        		
        	}else {
        		setStopAnimate(true);
        		

        		animateButton.setText("Start");
        		compImage.setCheckerboardAnimate(false);
        		

        		OKButton.setEnabled(true);
        		closeButton.setEnabled(true);
        		doCheckbox.setEnabled(true);
        		slider.setEnabled(true);
        		slider2.setEnabled(true);
        		speedSlider.setEnabled(true);
        		compImage.addMouseListener(compImage);
        		compImage.addMouseMotionListener(compImage);
        	}
        	
        	
        	
        }
    }

    /**
     * Sets values based on knob along slider.
     *
     * @param  e  Event that triggered this function.
     */
    public void stateChanged(ChangeEvent e) {
        int rowNumber, columnNumber;
        Object source = e.getSource();

        if (source == slider) {
            rowNumber = slider.getValue();
            if(rowNumber == 1 && slider2.getValue() == 1) {
            	slider.setValue(2);
            	slider.updateUI();
            }
            textRowNumber.setText(String.valueOf(rowNumber));
        }

        if (source == slider2) {
            columnNumber = slider2.getValue();
            if(columnNumber == 1 && slider.getValue() == 1) {
            	slider2.setValue(2);
            	slider2.updateUI();
            }
            textColumnNumber.setText(String.valueOf(columnNumber));
        }
    }

    /**
     * Sets up the GUI components of the dialog.
     */
    private void setup() {
        setForeground(Color.black);

        setTitle("Checkerboard pattern");

        JPanel paramPanel = new JPanel();

        paramPanel.setLayout(new BoxLayout(paramPanel, BoxLayout.Y_AXIS));
        paramPanel.setBorder(buildTitledBorder("Parameters"));

        doCheckbox = new JCheckBox("Use checkerboarding");
        doCheckbox.setFont(serif12);
        doCheckbox.setSelected(true);
        doCheckbox.setEnabled(true);
        doCheckbox.addActionListener(this);
        doCheckbox.setAlignmentX(Component.LEFT_ALIGNMENT);
        paramPanel.add(doCheckbox);

        JPanel rowPanel = new JPanel();

        labelRowNumber = new JLabel("Rows");
        labelRowNumber.setForeground(Color.black);
        labelRowNumber.setFont(serif12);
        labelRowNumber.setEnabled(true);
        rowPanel.add(labelRowNumber);

        rowPanel.add(Box.createHorizontalStrut(10));

        slider = new JSlider(1, maxRow, 2);
        slider.setFont(serif12);
        slider.setEnabled(true);
        slider.setMinorTickSpacing(5);
        slider.setPaintTicks(true);
        slider.addChangeListener(this);
        slider.setVisible(true);
        labelTable = new Hashtable<Integer,JLabel>();
        labelTable.put(new Integer(1), createLabel("1"));
        labelTable.put(new Integer(maxRow), createLabel(String.valueOf(maxRow)));
        slider.setLabelTable(labelTable);
        slider.setPaintLabels(true);
        rowPanel.add(slider);

        textRowNumber = new JTextField(String.valueOf(2), 4);
        textRowNumber.setFont(serif12);
        textRowNumber.setEnabled(false);
        textRowNumber.addFocusListener(this);
        rowPanel.add(textRowNumber);

        rowPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        paramPanel.add(rowPanel);

        JPanel columnPanel = new JPanel();

        labelColumnNumber = new JLabel("Columns");
        labelColumnNumber.setForeground(Color.black);
        labelColumnNumber.setFont(serif12);
        labelColumnNumber.setEnabled(true);
        columnPanel.add(labelColumnNumber);

        slider2 = new JSlider(1, maxColumn, 2);
        slider2.setFont(serif12);
        slider2.setEnabled(true);
        slider2.setMinorTickSpacing(5);
        slider2.setPaintTicks(true);
        slider2.addChangeListener(this);
        slider2.setVisible(true);
        labelTable2 = new Hashtable<Integer,JLabel>();
        labelTable2.put(new Integer(1), createLabel("1"));
        labelTable2.put(new Integer(maxColumn), createLabel(String.valueOf(maxColumn)));
        slider2.setLabelTable(labelTable2);
        slider2.setPaintLabels(true);
        columnPanel.add(slider2);

        textColumnNumber = new JTextField(String.valueOf(2), 4);
        textColumnNumber.setFont(serif12);
        textColumnNumber.setEnabled(false);
        textColumnNumber.addFocusListener(this);
        columnPanel.add(textColumnNumber);

        columnPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        paramPanel.add(columnPanel);
        
        
        
        JPanel speedPanel = new JPanel();
        speedPanel.setLayout(new BoxLayout(speedPanel, BoxLayout.Y_AXIS));
        speedPanel.setBorder(buildTitledBorder("Animate"));
        
        JPanel panel = new JPanel();
        
        speedLabel = new JLabel("Speed");
        speedLabel.setForeground(Color.black);
        speedLabel.setFont(serif12);
        speedLabel.setEnabled(false);
        
        panel.add(speedLabel);
        
        speedSlider = new JSlider(1, 10, 3);
        speedSlider.setFont(serif12);
        speedSlider.setEnabled(false);
        speedSlider.setMinorTickSpacing(5);
        speedSlider.setPaintTicks(true);
        speedSlider.addChangeListener(this);
        speedSlider.setVisible(true);
        speedLabelTable = new Hashtable<Integer,JLabel>();
        speedLabelTable.put(new Integer(1), createLabel("1"));
        speedLabelTable.put(new Integer(10), createLabel("10"));
        speedSlider.setLabelTable(speedLabelTable);
        speedSlider.setPaintLabels(true);
        
        panel.add(speedSlider);
        
        
        animateButton = new JButton("Start");
        animateButton.setActionCommand("animate");
        animateButton.addActionListener(this);
        animateButton.setEnabled(false);
        
        panel.add(animateButton);
        panel.setAlignmentX(Component.LEFT_ALIGNMENT);
        speedPanel.add(panel);

        getContentPane().add(paramPanel, BorderLayout.NORTH);
        getContentPane().add(speedPanel, BorderLayout.CENTER);
        JPanel buttonPanel = new JPanel();

        buildOKButton();
        OKButton.setText("Apply");
        OKButton.setActionCommand("OK");
        buttonPanel.add(OKButton);

        closeButton = new JButton("Close");
        closeButton.setMinimumSize(MipavUtil.defaultButtonSize);
        closeButton.setPreferredSize(MipavUtil.defaultButtonSize);
        closeButton.setFont(serif12B);
        buttonPanel.add(closeButton);
        closeButton.addActionListener(this);
        closeButton.setActionCommand("Close");

        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setVisible(true);
    }
    
    
    
    public synchronized boolean isStopAnimate() {
		return stopAnimate;
	}

	public synchronized void setStopAnimate(boolean stopAnimate) {
		this.stopAnimate = stopAnimate;
	}
	
	
	
	
	



	public synchronized int getCc() {
		return cc;
	}

	public synchronized void setCc(int cc) {
		this.cc = cc;
	}








	public synchronized boolean isThreadStopped() {
		return isStopped;
	}
	
	
	
	 /**
     * Cleans up the frame before closing.
     * 
     * @param event the window event that triggered this method
     */
    public void windowClosing(final WindowEvent event) {
    	if(animateThread != null && animateThread.isAlive()) {
    		setStopAnimate(true);
    		
    		while(!isThreadStopped()) {
				//do nothing
			}
    		compImage.addMouseListener(compImage);
    		compImage.addMouseMotionListener(compImage);
    		dispose();
    	
    		
    	}
    }








	public class Animate extends Thread {
		
		public void run() {
			//int i = 0;
			//int cc = compImage.getCheckerboardCounter();
			pixBufferB = compImage.getPixBufferB();
			cleanImageBufferB = compImage.getCleanImageBufferB();
			ySep = compImage.getySep();
			maxExtents = compImage.getMaxExtents();
			isStopped = false;

    		while(!isStopAnimate()) {
    			animateCheckerboard();
    			compImage.paintComponent(compImage.getGraphics());
    			cc = cc + 1;
    			if(cc == ySep) {
    				cc = 0;
    			}
    			//compImage.setCheckerboardCounter(cc);
    			try{
    				Thread.sleep(10);
    			}catch(InterruptedException e) {
    				break;
    			}
    		}
    		
    		isStopped = true;

    		
		}
		
		
		
		
		 private void flip(int x, int y, int dim) {

		    	if(pixBufferB[x + (y * dim)] == 0) {
		        	pixBufferB[x + (y * dim)] = cleanImageBufferB[x + (y * dim)];
		        }else {
		        	pixBufferB[x + (y * dim)] = 0;
		        }
		    }
		    
		    
		    private void animateCheckerboard() {
		    	int xDim, yDim;
		    	xDim = maxExtents[0];
		        yDim = maxExtents[1];
		        //int cc = getCheckerboardCounter();
		        int y =0;
		        int x = 0;

		        
		    	if(columnNumber == 1) {

			    		for (y = 0; y < yDim;) {
			
			                for (x = 0; x < xDim; x++) {
			                		
			                	if(y <= cc) {
			                		if(y == cc) {
			                			flip(x,y,xDim);
			                		}
			                	}else {
			                		flip(x,y,xDim);
			                	}
		     
			                }
			                
			                if(y < cc) {
		                		
		                		y++;
		                	}else {
		                		y = y + ySep;
		                	}
			                
			    		}
		    		
		                        
		                        
		    		
		    	}else if(rowNumber == 1) {
		    		
		    	}
		    	
		    }
    	
    }

}
