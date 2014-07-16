package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.Hashtable;
import java.util.Set;
import java.util.Vector;

import javax.swing.*;


/**
 * REALLY like JDialogRemoveSlices except it does not have an "algorithm"
 *
 * <p>builds a simple dialog which imbeds a JPanelAnonymizeImage. The JDialogAnonymizeImage will gray-out all entries
 * which are not available in the FileInfoDicom that goes with the given image. "Okay" then proceeds to remove or filter
 * the selected tags from the image.</p>
 *
 * @author   parsonsd
 * @version  0.9
 * @see      JDialogRemoveSlices
 */
public class JDialogAnonymizeImage extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -5845185607194185354L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JPanelAnonymizeImage checkboxPanel; //
    
    private JPanelAnonymizePrivateTags privateTagsPanel;

    /** DOCUMENT ME! */
    private ModelImage image;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * constructor to build a dialog allowing user to find which tags are available to anonymize.
     *
     * @param  parent  Parent dialog
     * @param  img     Image to anonymize
     */
    public JDialogAnonymizeImage(Dialog parent, ModelImage img) {
        super(parent, true); // make modal

        if (!img.isDicomImage() && !img.isMincImage()) {
            return;
        }

        image = img;
        setTitle("Anonymize sensitive info");
        setForeground(Color.black);

        JTabbedPane tabs = new JTabbedPane();
        
        
        // place a check-box list in here
        checkboxPanel = new JPanelAnonymizeImage();

        if (img.isDicomImage()) {
            checkboxPanel.setDicomInfo((FileInfoDicom) img.getFileInfo(0));
        } else if (img.isMincImage()) {
            checkboxPanel.setMincInfo((FileInfoMinc) img.getFileInfo(0));
        }

        mainDialogPanel.add(checkboxPanel, BorderLayout.CENTER);
        
        //getContentPane().add(mainDialogPanel);
        
        tabs.insertTab("Tag options", null, mainDialogPanel, "Tag Selection", 0);
        
        privateTagsPanel = new JPanelAnonymizePrivateTags(img);
        tabs.insertTab("Private tag options", null, privateTagsPanel, "Private Tag Selection", 1);
        
        getContentPane().add(tabs);
        getContentPane().add(getOKCancelPanel(), BorderLayout.SOUTH);
        
        setResizable(true); // since locations are hard-coded we are not checking for different sizes. prevent user from
                            // changing
        addWindowListener(this); // check for events
        pack();
        setSize(400, 400);
        setVisible(true); // let someone see the dialog.
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and does the routine.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource(); // whatever the user clicked on

        if (source == OKButton) { // if user pressed "expurgate" ...

            int anonymizeChoice;

            if (checkboxPanel.getNumberSelected() == 0) {
                MipavUtil.displayError("No fields were selected!  Select a field.");
            } else {

                try {
                    JOptionPane.getRootFrame().setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
                } catch (FileNotFoundException error) {
                    Preferences.debug("Exception ocurred while getting <" + error.getMessage() +
                                      ">.  Check that this file is available.\n");
                    System.err.println("Exception ocurred while getting <" + error.getMessage() +
                                       ">.  Check that this file is available.\n");
                }
                
                //Need to find sequence tags and use it for both the public and private tag methods

                anonymizeChoice = JOptionPane.showConfirmDialog(null,
                                                                "You are about to make a permament change to the entire image.\n" +
                                                                "Are you sure you want to remove the sensitive data?",
                                                                "Confirm for Anonymize", JOptionPane.YES_NO_OPTION,
                                                                JOptionPane.QUESTION_MESSAGE);

                if (anonymizeChoice == JOptionPane.YES_OPTION) {
                	
                	Vector<FileDicomSQItem> seqTags = getSequenceTags();
                	
                    image.anonymize(checkboxPanel.getSelectedList(), true); // anonymize the image of sensitive data
                    image.anonymizeSequenceTags(checkboxPanel.getSelectedList(), seqTags);
                    
                    FileDicomKey[] keys = privateTagsPanel.getSelectedKeys();
                    if(keys != null){
                    	image.removePrivateTags(keys);
                    	image.removePrivateSequenceTags(keys, seqTags);
                    }
                    
                    setVisible(false); // Hide dialog
                }
            }
        } else if (source == cancelButton) {
            dispose();
        } else {
            super.actionPerformed(event);
        }
    }

    /**
     * makes the panel which displays the ok and cancel buttons.
     *
     * @return  DOCUMENT ME!
     */
    protected JPanel getOKCancelPanel() {
        JPanel okCancelPanel = new JPanel();

        // Make & set the OK (purge) and Cancel buttons--place outside the border
        buildOKButton();

        // if there are no fields which the user may select, turn off the OK button
        if (checkboxPanel.getNumberVisible() == 0) { // if no tags are visible
            OKButton.setEnabled(false);
        }

        okCancelPanel.add(OKButton, BorderLayout.WEST);
        buildCancelButton();
        okCancelPanel.add(cancelButton, BorderLayout.EAST);

        return okCancelPanel;
    }
    
    private Vector<FileDicomSQItem> getSequenceTags(){
    	
    	Vector<FileDicomSQItem> seqVec = new Vector<FileDicomSQItem>();
    	FileInfoDicom info = (FileInfoDicom)image.getFileInfo()[0];
    	Hashtable<FileDicomKey, FileDicomTag> hash = info.getTagTable().getTagList();
    	Set<FileDicomKey> keys = hash.keySet();
    	for(FileDicomKey k : keys){
    		Object obj = hash.get(k).getValue(false);
    		if(obj instanceof FileDicomSQ){
    			FileDicomSQ seq = (FileDicomSQ) obj;
    			Vector<FileDicomSQItem> vec = seq.getSequence();
    			seqVec.addAll(vec);
    		}
    	}
    	
    	return seqVec;
    	
    }
}
