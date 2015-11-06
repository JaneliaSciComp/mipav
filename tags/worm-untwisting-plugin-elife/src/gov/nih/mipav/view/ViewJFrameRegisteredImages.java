package gov.nih.mipav.view;


import gov.nih.mipav.model.provenance.ProvenanceRecorder;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.actions.*;
import gov.nih.mipav.model.structures.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import java.util.*;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;


/**
 * Contains a seperatly running thread which checks the list of registered images. The list is updated at a rate which
 * is user-specified. The frame will allow the user to adjust how frequently the list will be updated. For efficiency,
 * no updating will occur if the frame has been minimized.
 *
 * <p>The registered images frame provides direct user access to the function Runtime.getRuntime.gc() via a "Garbage
 * Collector" button.</p>
 *
 * <p>There are a number of possible supported uses. There is an unimplemented button to remove un-framed images from
 * the ViewUserInterface image hashtable and there is a button to bring framed-images to the front.</p>
 *
 * <p>This class has a number of inside classes:</p>
 *
 * <ul>
 *   <li>ImageRegistryMonitor (a type of Thread) to update the list of images registered with the ViewUserInterface</li>
 *   <li>MouseClickAdapter (a type of MouseAdapter) to attempt to bring the selected image's frame to the front</li>
 *   <li>ImageCellRenderer (a type of JLabel) to present both the image-name and a helpful icon depicting whether or not
 *     the associated image is in a frame or not</li>
 * </ul>
 *
 * @version  15 April 2002
 * @author   Lynne M. Pusanik
 * @author   Matthew J. McAuliffe, Ph.D.
 * @author   David A. Parsons
 */
public class ViewJFrameRegisteredImages extends JFrame
        implements ActionListener, ListSelectionListener, ChangeListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -4417297294466318211L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Buttons for dealing with image deletions */
    private JButton callImageDeleteButton, callImageDeleteAllButton;

    /** Button for dealing with garbage collection */
    private JButton callGCButton;

    /** Button for bringing frame to front */
    private JButton callFrameToFrontButton;
    
    /** Buttons for dealing with frame deletion. */
    private JButton callFrameDeleteButton, callFrameDeleteAllButton;

    /** The list of images and frames */
    private JList imageList;

    /** Button for pausing updating of currently registered images */
    private JButton pauseButton;

    /** Whether currently registered images list is updating */
    private boolean paused = false; // don't update list when true

    /** The rate at which to update imageList */
    private JTextField sampleRate; // user control over update period

    /** The scroll pane containing the image list */
    private JScrollPane scrollPane;

    /** thread to watch image registry. */
    private ImageRegistryMonitor surf;

    /** The current user interface */
    private ViewUserInterface UI;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor.
     */
    public ViewJFrameRegisteredImages() {
        super();
        UI = ViewUserInterface.getReference();
        setTitle("Image Registry Monitor");

        try {
            setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
        } catch (FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage() +
                              ">.  Check that this file is available.\n");
            System.err.println("Exception ocurred while getting <" + error.getMessage() +
                               ">.  Check that this file is available.\n");
        }

        this.addWindowListener(new WindowAdapter() {
                public void windowClosing(WindowEvent we) {
                    surf.stop();
                    dispose();
                }

                public void windowIconified(WindowEvent we) {
                    paused = false;
                    pauseButton.doClick();
                }

                public void windowDeiconified(WindowEvent we) {
                    paused = true;
                    pauseButton.doClick();
                }
            });
        this.getContentPane().setLayout(new BorderLayout());

        TitledBorder border;

        JPanel userPanel = new JPanel(new BorderLayout());

        // panel is titled & etched
        JPanel setupPanel = new JPanel(new BorderLayout());
        border = new TitledBorder("Sampling");
        border.setTitleColor(Color.black);
        border.setTitleFont(MipavUtil.font12B);
        border.setBorder(new EtchedBorder());
        setupPanel.setBorder(border);

        pauseButton = new JButton("Pause");
        pauseButton.setFont(MipavUtil.font12B);
        pauseButton.addActionListener(this);
        setupPanel.add(pauseButton, BorderLayout.NORTH);

        JPanel samplePanel = new JPanel();
        JLabel labelSampleRate = new JLabel("Update Rate"); // add name for user input
        labelSampleRate.setFont(MipavUtil.font12);
        labelSampleRate.setForeground(Color.black);
        samplePanel.add(labelSampleRate);
        samplePanel.add(Box.createHorizontalStrut(10)); // add spacing
        sampleRate = new JTextField("5000"); // add user input field
        makeNumericsOnly(sampleRate);
        sampleRate.setColumns(5);
        sampleRate.setHorizontalAlignment(JTextField.RIGHT);
        sampleRate.addActionListener(this);
        samplePanel.add(sampleRate);

        JLabel ms = new JLabel("ms"); // add sample rate unit
        ms.setFont(MipavUtil.font12);
        ms.setForeground(Color.black);
        samplePanel.add(ms);
        setupPanel.add(samplePanel, BorderLayout.SOUTH);

        userPanel.add(setupPanel, BorderLayout.NORTH);


        // get the current registered images
        @SuppressWarnings("unchecked")
        Vector<?> names = new Vector();

        // put the list together
        imageList = new JList(names);
        imageList.setToolTipText("Double-click to bring framed-images to front");
        imageList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        imageList.addListSelectionListener(this);
        imageList.addMouseListener(new MouseClickAdapter());
        imageList.setCellRenderer(new ImageCellRenderer());

        JPanel pan = new JPanel(new GridLayout(1, 1));
        border = new TitledBorder("Currently Registered Images");
        border.setTitleColor(Color.black);
        border.setTitleFont(MipavUtil.font12B);
        border.setBorder(new EtchedBorder());
        pan.setBorder(border);
        scrollPane = new JScrollPane(imageList);
        pan.add(scrollPane);
        userPanel.add(pan, BorderLayout.CENTER);
        this.getContentPane().add(userPanel, BorderLayout.CENTER);

       

        // The constructor below will work if the Delete button is added...:
        // JPanel buttonPan = new JPanel(new GridLayout(3,1));
        // ...other wise, use the following constructor:
        JPanel buttonPan = buildButtonPanel();

        

        this.getContentPane().add(buttonPan, BorderLayout.SOUTH);

        surf = new ImageRegistryMonitor();
        surf.addImageRegistryChangeListener(this);
        sampleRate.addFocusListener(surf);

        // start the registry checker
        surf.start();
        setVisible(true);
        setSize(330, 380);
        validate();

    }
    
    private JPanel buildButtonPanel() {
    	JPanel buttonPanel = new JPanel(new GridBagLayout());
    	GridLayout frameLayout = new GridLayout(3, 1);
    	JPanel framePanel = new JPanel(frameLayout);
    	framePanel.setBorder(MipavUtil.buildTitledBorder("Framed Image Actions"));
    	
    	frameLayout.setVgap(14);
    	
    	//make buttons for frame panel
    	callFrameDeleteButton = new JButton("Delete Frame");
        callFrameDeleteButton.setToolTipText("Delete the selected frame");
        callFrameDeleteButton.setFont(MipavUtil.font12B);
        callFrameDeleteButton.addActionListener(this);
        
        callFrameDeleteAllButton = new JButton("Delete All Frames");
        callFrameDeleteAllButton.setToolTipText("Delete all listed frames");
        callFrameDeleteAllButton.setFont(MipavUtil.font12B);
        callFrameDeleteAllButton.addActionListener(this);
        
        callFrameToFrontButton = new JButton("Bring Frame to Front");
        callFrameToFrontButton.setToolTipText("Brings the selected frame to the front");
        callFrameToFrontButton.setFont(MipavUtil.font12B);
        callFrameToFrontButton.addActionListener(this);
        
        framePanel.add(callFrameDeleteButton);
        framePanel.add(callFrameDeleteAllButton);
        framePanel.add(callFrameToFrontButton);
       
    	GridBagConstraints gbc = new GridBagConstraints();
    	gbc.gridx = 0;
    	gbc.gridy = 0;
    	gbc.gridheight = 2;
    	gbc.fill = GridBagConstraints.VERTICAL;
        buttonPanel.add(framePanel, gbc);
        
        JPanel imagePanel = new JPanel(new BorderLayout());
    	imagePanel.setBorder(MipavUtil.buildTitledBorder("Image Actions"));
    	
    	// make buttons for image panel
        callImageDeleteButton = new JButton("Delete Image");
        callImageDeleteButton.setToolTipText("Delete the selected image not in a frame");
        callImageDeleteButton.setFont(MipavUtil.font12B);
        callImageDeleteButton.addActionListener(this);
        
        callImageDeleteAllButton = new JButton("Delete All Images");
        callImageDeleteAllButton.setToolTipText("Delete all images that are not in frames");
        callImageDeleteAllButton.setFont(MipavUtil.font12B);
        callImageDeleteAllButton.addActionListener(this);

        imagePanel.add(callImageDeleteButton, BorderLayout.NORTH);
        imagePanel.add(callImageDeleteAllButton, BorderLayout.CENTER);
        
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.gridheight = 1;
        buttonPanel.add(imagePanel, gbc);
        
        JPanel memoryPanel = new JPanel(new GridBagLayout());
        memoryPanel.setBorder(MipavUtil.buildTitledBorder("Memory Actions"));
        GridBagConstraints gbcInner = new GridBagConstraints();
        
        //make buttons for memory panel
        callGCButton = new JButton("Free memory");
        callGCButton.setFont(MipavUtil.font12B);
        callGCButton.addActionListener(this);
        
        gbcInner.gridx = 0;
        gbcInner.gridy = 0;
        gbcInner.weightx = 1;
        gbcInner.fill = GridBagConstraints.HORIZONTAL;
        memoryPanel.add(callGCButton, gbcInner);
        
        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        buttonPanel.add(memoryPanel, gbc);
        
        return buttonPanel;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    // ************************************************************************
    // **************************** Action Events *****************************
    // ************************************************************************

    /**
     * Calls various methods based on the user's actions.
     *
     * @param  event  event that triggered function
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();

        if (source.equals(pauseButton)) {

            if (paused) { // if already paused.... change the sample rate, and unpause
                surf.start();
                paused = false;
                pauseButton.setText("Pause");
            } else { // not paused?  stop checking the memory, doing updates and tell user
                surf.stop();
                paused = true;
                pauseButton.setText("Resume");
            }
        } else if (source.equals(callGCButton)) { // call the garbage collector
            Runtime.getRuntime().gc();
            Runtime.getRuntime().runFinalization();

            if (paused) {
                surf.stop(); // should update the display & memory values
            }

            ScriptRecorder.getReference().addLine(new ActionCollectGarbage());
            ProvenanceRecorder.getReference().addLine(new ActionCollectGarbage());
        } else if (source.equals(callImageDeleteButton)) {

            // selectedName = get selected item from the list
            String selectedName = (String) imageList.getSelectedValue();
            deleteItem(selectedName, false);
            
        } else if(source.equals(callFrameDeleteButton)) {
        	
        	// selectedName = get selected item from the list
            String selectedName = (String) imageList.getSelectedValue();
            deleteItem(selectedName, true);
        		
    	} else if(source.equals(callImageDeleteAllButton) || source.equals(callFrameDeleteAllButton)) { 
    		
    		boolean doFrames = source.equals(callFrameDeleteAllButton);
    		ListModel imageModel = imageList.getModel();
    		for(int i=0; i<imageModel.getSize(); i++) {
    			String name = (String)imageModel.getElementAt(i);
    			deleteItem(name, doFrames);
    		}
    		
    	} else if (source.equals(callFrameToFrontButton)) { // bring image to front

            String selectedName = (String) imageList.getSelectedValue();

            if (selectedName == null) {
                return;
            }

            try {
                imageToFront(selectedName);
            } catch (NullPointerException npe) {
                MipavUtil.displayError("There is no associated image-frame!\n" + "Can't bring " + selectedName +
                                       " to front");
                Preferences.debug("Image " + selectedName + " was not associated " +
                                  "with an image frame.  Either the image is " +
                                  "still in use, or it was not deleted in " + "error.\n"); // log.
            } catch (IllegalArgumentException iae) {
                Preferences.debug("Illegal Argument Exception in " +
                                  "ViewJFrameRegisteredImages when clicking on ToFront. " +
                                  "Somehow the Image list sent an incorrect name to " +
                                  "the image image hashtable.  \n", 2);
                System.out.println("Bad argument.");
            }
        }
    }
    
    /**
     * Deletes the item specified by name.  If false, only the model image is deleted
     * @param name the object to delete
     * @param deleteFrame whether the frame should be deleted along with the image
     */
    private void deleteItem(String name, boolean deleteFrame) {
    	// System.out.println("selected name = " + selectedName);
        if (name == null) {
            return; // log nothing.
        }

        try {
            ModelImage image = UI.getRegisteredImageByName(name);
            ViewJFrameImage frame = UI.getFrameContainingImage(image);

            // An image that has a frame is only deleted when deleteFrame == true
            if (image != null && 
            		((deleteFrame && frame != null) || (!deleteFrame && frame == null))) {
                image.disposeLocal();
                
                if(deleteFrame && frame != null) {
                	frame.close();
                }
            }
            
            Runtime.getRuntime().gc();
            Runtime.getRuntime().runFinalization();
        } catch (IllegalArgumentException iae) {

            // MipavUtil.displayError("There was a problem with the " +
            // "supplied name.\n" );
            Preferences.debug("Illegal Argument Exception in " +
                              "ViewJFrameRegisteredImages when clicking on Delete. " +
                              "Somehow the Image list sent an incorrect name to " +
                              "the image image hashtable.  \n" + iae.getMessage() + "\n", 1);
            // System.out.println("Bad argument.");
        }
    }

    /**
     * Shows the frame with the memory.
     *
     * @param  flag  DOCUMENT ME!
     */
    public synchronized void setVisible(boolean flag) {
        setLocation(50, 50);
        super.setVisible(flag);
    }

    // ************************************************************************
    // **************************** Change Events *****************************
    // ************************************************************************

    /**
     * Calls various methods based on the changes in the memory panel.
     *
     * @param  event  event that triggered function
     */
    public void stateChanged(ChangeEvent event) {
        Object source = event.getSource();
        ImageRegistryMonitor mon = (ImageRegistryMonitor) source;

        // update the list in the window
        Vector<String> names = mon.getRegisteredNames();
        
        int selected = -1;
        if(imageList.getModel().getSize() > 0) {
	        selected = imageList.getSelectedIndex();
	        if(selected != -1) {
		        Object selectedObj = imageList.getSelectedValue();
		        
		        if(selectedObj != null) {
		        	selected = names.indexOf(selectedObj);
		        }
	        }
        }
        imageList.setListData(names);
        if(selected != -1) {
        	imageList.setSelectedIndex(selected);
        }
        	
        repaint();
    }

    // ***********************************************************************
    // *********************** ListSelectionEvents****************************
    // ***********************************************************************
    /**
     * Whenever the list changes, the valueChanged is called.
     *
     * <p>calls <code>callToFrontbutton#doClick()</code> when the list has been double-clicked, to bring the selected
     * image (and associated frame) to the front.</p>
     *
     * @param  event  DOCUMENT ME!
     */
    public void valueChanged(ListSelectionEvent event) {
        //        if (event.getValueIsAdjusting()) {            System.out.println("...is adjusting...."); return;  }
        //     JList list = (JList)event.getSource();        System.out.println("value changed.");      if
        // (list.isSelectionEmpty()) {            return;        }        String selectedName = (String)
        // list.getSelectedValue();        imageToFront(selectedName);        System.out.println("ValueChanged");
    }


    /**
     * Takes a txt field, and forces the textfield to accept numbers, backspace and delete-key entries.
     *
     * <p>also tells the pauseButton to click.</p>
     *
     * @param  txt  DOCUMENT ME!
     */
    protected void makeNumericsOnly(JTextField txt) {
        txt.addKeyListener(new KeyAdapter() { // make the field
                public void keyTyped(KeyEvent evt) { // not accept letters

                    // JTextField t = (JTextField) evt.getComponent();
                    char ch = evt.getKeyChar();

                    if (((ch < '0') || (ch > '9')) && ((ch != KeyEvent.VK_DELETE) && (ch != KeyEvent.VK_BACK_SPACE))) {

                        // if is the case that ch is outside the bounds of a number
                        // AND it is the case that ch is neither a BS or a DE, then
                        // key is not a digit or a deletion char
                        evt.consume();
                    } else {
                        paused = false;
                        pauseButton.doClick();
                    }
                }
            });
    }


    /**
     * Using the supplied name as the image name, this method finds the frame associated with the image and brings it to
     * the front. Does nothing when selectedName is <CODE>null</CODE>.
     *
     * @param   selectedName  DOCUMENT ME!
     *
     * @throws  NullPointerException      when the <CODE>selectedName</CODE> is in the image list, but not associated
     *                                    with any frame.
     * @throws  IllegalArgumentException  if the <CODE>selectedName</CODE> cannot be found in the image list
     */
    private void imageToFront(String selectedName) throws NullPointerException, IllegalArgumentException {

        if (imageList.isSelectionEmpty()) {
            return; // no selected name.  fail quietly.
        }

        try {
            UI.getFrameContainingImage(UI.getRegisteredImageByName(selectedName)).toFront();
        } catch (IllegalArgumentException iae) {

            // MipavUtil.displayError("There was a problem with the supplied name.\n" );
            Preferences.debug("Illegal Argument Exception in " + "ViewJFrameRegisteredImages.imageToFront() " +
                              "Somehow the Image list sent an incorrect name to " + "the image image hashtable. " +
                              "\n", 1);
            System.out.println("Bad argument.");
        }

    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Identifies components that can be used as "rubber stamps" to paint the cells in a JList.
     *
     * <p>Images which are shown to have an associated frame (only Image A) will show the "frame.gif" icon. All others
     * will show the "rect.gif" icon.</p>
     *
     * @see  ViewUserInterface#getFrameContainingImage()
     */
    // if this class becomes public and is no longer "inner",
    // then there will need to be more arguments in the constructor...
    private class ImageCellRenderer extends JLabel implements ListCellRenderer {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = -5908802887298772112L;

        /** DOCUMENT ME! */
        private ImageIcon floating = MipavUtil.getIcon("rect.gif");

        /** DOCUMENT ME! */
        private ImageIcon frame = MipavUtil.getIcon("frame.gif");

        /**
         * Identifies components that can be used as "rubber stamps" to paint the cells in a JList. Preset the label to
         * "opaque" true.
         */
        // if this class becomes public and is no longer "inner",
        // then there will need to be more arguments in the constructor...
        public ImageCellRenderer() {
            setOpaque(true);
        }

        /**
         * Return a component that has been configured to display the specified value. That component's <code>
         * paint</code> method is then called to "render" the cell. If it is necessary to compute the dimensions of a
         * list because the list cells do not have a fixed size, this method is called to generate a component on which
         * <code>getPreferredSize</code> can be invoked.
         *
         * <p>Images which are shown to have an associated frame (only Image A) will show the "frame.gif" icon. All
         * others will show the "rect.gif" icon.</p>
         *
         * @param   list          The JList we're painting.
         * @param   value         The value returned by list.getModel().getElementAt(index).
         * @param   index         The cells index.
         * @param   isSelected    True if the specified cell was selected.
         * @param   cellHasFocus  True if the specified cell has the focus.
         *
         * @return  A component whose paint() method will render the specified value.
         *
         * @see     ViewUserInterface#getFrameContainingImage()
         * @see     JList
         * @see     ListSelectionModel
         * @see     ListModel
         */
        public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected,
                                                      boolean cellHasFocus) {
            String name = value.toString();
            setText(name);

            ModelImage img = null;

            try {
                img = UI.getRegisteredImageByName(name);
            } catch (IllegalArgumentException iae) {
                Preferences.debug("Requested an image from registry monitor that is not registered: " + name);

                return this;
            }

            // if there is no associated frame with the name, use the "floating"
            // icon, else, we'll show it as having a "Frame" icon.
            // note: IMAGE B is not found to have an associated frame!
            if (UI.getFrameContainingImage(img) == null) {
                setIcon(floating);
            } else {
                setIcon(frame);
            }

            // choose coloration
            if (isSelected) {
                setBackground(list.getSelectionBackground());
                setForeground(list.getSelectionForeground());
            } else {
                setBackground(list.getBackground());
                setForeground(list.getForeground());
            }

            setEnabled(list.isEnabled());
            setFont(list.getFont());
            setOpaque(true);

            return this;
        }

    } // end ImageCellRenderer

    /**
     * As an extension of MouseAdapter, this class merely responds on clicked list events. In particular, the
     * double-clicked item is brought to the front if the image is associated with an image frame.
     *
     * @see  ViewJFrameRegisteredImages#bringToFront(String)
     */
    private class MouseClickAdapter extends MouseAdapter {

        /**
         * Responds only on double-clicked events from the JList. The double-clicked list item brings to the front the
         * frame of the associated image.
         *
         * <p>Ignores ClassCastExceptions, and will present to the user the error messages if there are
         * NullPointerExceptions.</p>
         *
         * @see  ViewJFrameRegisteredImages#bringToFront(String)
         */
        public void mouseClicked(MouseEvent event) {

            if (event.getClickCount() == 2) {
                String selectedName = "''";

                try {
                    JList list = (JList) event.getSource();

                    if (list.isSelectionEmpty()) {
                        return;
                    }

                    selectedName = (String) list.getSelectedValue();
                    imageToFront(selectedName);
                } catch (ClassCastException cce) {
                    Preferences.debug("ViewJFrameRegisteredImages." + "MouseClickAdapter tried to handle " +
                                      "something that wasn't a " + "javax.swing.JList.\n", 4);
                } catch (NullPointerException npe) {
                    MipavUtil.displayError("There is no associated " + "image-frame!\n" + "Can't bring " +
                                           selectedName + " to front");
                    Preferences.debug("Image " + selectedName + " was not " + "associated with an image frame.  " +
                                      "Either the image is still in use, " + "or it was not deleted in " + "error.\n"); // log.
                } catch (IllegalArgumentException iae) {
                    Preferences.debug("Illegal Argument Exception in " +
                                      "ViewJFrameRegisteredImages when clicking on " +
                                      "ToFront.  Somehow the Image list sent " +
                                      "an incorrect name to the image image hashtable." + "\n", 2);
                }
            }
        }
    } // end class MouseClickAdapter

} // end class ViewJFrameRegisteredImages
