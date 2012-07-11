package gov.nih.mipav.view.dialogs.reportbug;

import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.GuiBuilder;
import gov.nih.mipav.view.dialogs.JDialogBase;

import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;
import java.awt.datatransfer.*;

import java.io.*;

import javax.swing.*;
import javax.swing.event.*;

public class ImageAttacher extends JDialogBase implements MouseListener{
	

    /**
	 * 
	 */
	private static final long serialVersionUID = 8736666389560986015L;

	private JLabel instructions;
	private JLabel instructions2;
	
	/** The rectangle that will be captured from the screen to save to a file. */
    private Rectangle currentRectangle;
	
	/** Old glass panes to reset to after drawing is done. */
    private Component[] oldPanes;
    
    /**
     * Special glass panes for all the valid frames in the GUI so that the user can draw rectangles on top of objects in
     * the GUI.
     */
    //private MyGlassPane[] myGlassPanes;
	
	public ImageAttacher(){
		createAndShowGUI();
	}
	

	@Override
	public void actionPerformed(ActionEvent event) {
//		String command = event.getActionCommand();
//
//        if (command.equals("OK")) {
//
//            if ((currentRectangle != null) && !currentRectangle.isEmpty() &&
//                    (currentRectangle.x > -1) && (currentRectangle.y > -1)) {
//
//                if (writeImage()) {
//                	
//                	//commented the code below out in order to leave dialog up so that you can 
//                	//select multiple regions one after the other
//                	
//                    //for (int i = 0; i < oldFrames.length; i++) {
//                    //   myGlassPanes[i].setVisible(false);
//                    //   oldFrames[i].setGlassPane(oldPanes[i]);
//                    //   oldFrames[i].removeWindowListener(this);
//                    //   oldFrames[i].removeMouseListener(this);
//                    //   myGlassPanes[i] = null;
//                    //}
//
//                    //myGlassPanes = null;
//                    //dispose();
//                    //System.gc();
//                	
//                }
//            } else {
//                MipavUtil.displayError("You must choose a region or window to capture.");
//            }
//        } else if (command.equals("Cancel")) {
//
//            for (int i = 0; i < oldFrames.length; i++) {
//                myGlassPanes[i].setVisible(false);
//                oldFrames[i].setGlassPane(oldPanes[i]);
//                oldFrames[i].removeWindowListener(this);
//                oldFrames[i].removeMouseListener(this);
//                myGlassPanes[i] = null;
//            }
//            
//            myGlassPanes = null;
//            dispose();
//            System.gc();
//        }
	}

	private void createAndShowGUI(){
		GuiBuilder ref = new GuiBuilder(this);
		
		GridBagConstraints gbc = new GridBagConstraints();
    	gbc.gridwidth = 1;
    	gbc.gridheight = 1;
    	gbc.anchor = GridBagConstraints.WEST;
    	gbc.weightx = 1;
    	
        instructions = new JLabel("Select the window that you would like to attach");
        instructions.setFont(MipavUtil.font12);
        instructions.setForeground(Color.black);
        
        JPanel instruction = new JPanel();
        instruction.setLayout(new BoxLayout(instruction, BoxLayout.Y_AXIS));
        instruction.add(instructions);

        JPanel buttonPanel = new JPanel();
        buttonPanel.add(OKButton);
        buttonPanel.add(cancelButton);

        JPanel mainPanel = new JPanel();
        mainPanel.setLayout(new BorderLayout());
        mainPanel.add(instructions, BorderLayout.NORTH);
        mainPanel.add(buttonPanel, BorderLayout.SOUTH);

        getContentPane().add(mainPanel);
        
        pack();

	}
	
//    /**
//     * Our special glass panes draw rectangles based on how the user traces them using the mouse. The mouse events
//     * aren't captured unless the glass pane is visible.
//     */
//    class MyGlassPane extends JComponent {
//
//        /** Use serialVersionUID for interoperability. */
//        private static final long serialVersionUID = -1351237665634271651L;
//
//        /** When we're done drawing the rectangle, change the color. */
//        boolean complete = false;
//
//        /** Dimensions of rectangle to draw. */
//        int x = -1, y = -1, width = -1, height = -1;
//
//        /**
//         * Creates new glass pane and adds the listeners.
//         */
//        public MyGlassPane() {
//            GlassPaneListener listener = new GlassPaneListener(this);
//            addMouseListener(listener);
//            addMouseMotionListener(listener);
//        }
//
//        /**
//         * Paints a yellow rectangle based on the dimensions if not complete; if complete, paints a red rectangle.
//         *
//         * @param  g  Graphics component for this paint.
//         */
//        public void paint(Graphics g) {
//
//            if (x > 0) {
//
//                if (!complete) {
//                    g.setColor(Color.yellow);
//                } else {
//                    g.setColor(Color.red);
//                }
//
//                g.drawRect(x, y, width, height);
//            }
//        }
//
//        /**
//         * Accessor to tell the glass pane that the rectangle drawing is complete. If so, sets the currentRectangle
//         * appropriately.
//         *
//         * @param  comp  <code>true</code> if drawing is complete, <code>false</code> otherwise.
//         */
//        public void setComplete(boolean comp) {
//            this.complete = comp;
//
//            if (complete) {
//
//                // the +1 accounts for the size of the line
//                Point p = new Point(x + 1, y + 1);
//                SwingUtilities.convertPointToScreen(p, this);
//
//                // the -1 accounts for the size of the line
//                Dimension d = new Dimension(width - 1, height - 1);
//                currentRectangle = new Rectangle(p, d);
//            }
//        }
//
//        /**
//         * Accessor to set the height value of the painted rectangle.
//         *
//         * @param  h  Height value of painted rectangle.
//         */
//        public void setH(int h) {
//            this.height = h;
//        }
//
//        /**
//         * Accessor to set the width value of the painted rectangle.
//         *
//         * @param  w  Width value of painted rectangle.
//         */
//        public void setW(int w) {
//            this.width = w;
//        }
//
//        /**
//         * Accessor to set the x value of the painted rectangle.
//         *
//         * @param  x  X value of painted rectangle.
//         */
//        public void setX(int x) {
//            this.x = x;
//        }
//
//        /**
//         * Accessor to set the y value of the painted rectangle.
//         *
//         * @param  y  Y value of painted rectangle.
//         */
//        public void setY(int y) {
//            this.y = y;
//        }
//    }

	@Override
	public void mouseClicked(MouseEvent arg0) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void mouseEntered(MouseEvent arg0) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void mouseExited(MouseEvent arg0) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void mousePressed(MouseEvent arg0) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void mouseReleased(MouseEvent arg0) {
		// TODO Auto-generated method stub
		
	}
}
