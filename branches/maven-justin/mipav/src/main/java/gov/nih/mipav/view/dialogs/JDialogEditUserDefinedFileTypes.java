package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.FileTypeTable;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewImageFileFilter;

import java.awt.BorderLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.TreeSet;

import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;


/**
 * This class is the dialog that allows the user to edit the user defined file extensions filter
 * 
 * @author Nish Pandya
 */
public class JDialogEditUserDefinedFileTypes extends JDialogBase {

    /** The panels that make up this Dialog */
    private JPanel displayPanel, applyClosePanel;

    /** This is the ArrayList of JCheckBoxes */
    private ArrayList<JCheckBox> checkBoxArrList = new ArrayList<JCheckBox>();

    /**
     * This is a list of the checkbox names that is used for validation of user input to make sure there are no
     * duplicates
     */
    private ArrayList<String> checkboxNames = new ArrayList<String>();

    /** This a list is the list of checked check boxes that is populated when user hits apply */
    private ArrayList<String> checkedFileTypes = new ArrayList<String>();

    /** This is the alphabetically sorted collection of supported file extension names */
    private TreeSet<String> fileExtensionsTS = new TreeSet<String>(FileTypeTable.getAllFileTypeExtensions());

    /** This is the user input for additional file extensions */
    private JTextField userInput;

    /** This array is the list of additional file extensions that user typed in that is populated when user hits apply */
    private String[] userInputFileTypes = null;

    /**
     * This array is the list of file extensions that is pulled from the Preferences. We need this in order to check the
     * appropriate check boxes and the user input text field when the user opens up this dialog initailly
     */
    private String[] preferencesFileTypes = null;

    /**
     * Constructor.
     */
    public JDialogEditUserDefinedFileTypes() {

        setTitle("Edit User Defined Filter");

        // get the user defined file types preferences
        if (Preferences.getProperty(Preferences.PREF_USER_FILETYPES) != null) {
            if ( !Preferences.getProperty(Preferences.PREF_USER_FILETYPES).trim().equals("")) {
                preferencesFileTypes = Preferences.getProperty(Preferences.PREF_USER_FILETYPES).split("; ");
            }
        }

        displayPanel = new JPanel();
        GridBagLayout gbl = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();
        displayPanel.setLayout(gbl);

        int x = 0;
        int y = 0;
        Iterator<String> iter = fileExtensionsTS.iterator();
        while (iter.hasNext()) {
            String extensionLabel = (String) "*" + iter.next();
            JCheckBox checkBox = new JCheckBox(extensionLabel);
            checkBoxArrList.add(checkBox);
            checkboxNames.add(extensionLabel);
            if (checkPreference(extensionLabel)) {
                checkBox.setSelected(true);
            }
            renderCheckBox(checkBox, gbl, gbc, x, y);
            if (x < 4) {
                x++;
            } else {
                x = 0;
                y++;
            }
        }

        gbc.gridx = 0;
        gbc.gridy = ++y;
        gbc.gridwidth = 5;
        gbc.insets = new Insets(10, 5, 10, 5);
        JLabel label1 = new JLabel("Enter additional file extensions below. ( format: *.abc;*.def;*.ghi )");
        gbl.setConstraints(label1, gbc);
        displayPanel.add(label1);

        gbc.gridx = 0;
        gbc.gridy = ++y;
        gbc.gridwidth = 5;
        gbc.insets = new Insets(10, 5, 10, 5);
        userInput = new JTextField(30);

        if (Preferences.getProperty(Preferences.PREF_USER_FILETYPES_TEXTFIELDS) != null) {
            userInput.setText(Preferences.getProperty(Preferences.PREF_USER_FILETYPES_TEXTFIELDS));
        }
        gbl.setConstraints(userInput, gbc);
        displayPanel.add(userInput);

        applyClosePanel = new JPanel();
        buildOKButton();
        OKButton.setText("Apply");
        applyClosePanel.add(OKButton, BorderLayout.WEST);
        buildCancelButton();
        cancelButton.setText("Cancel");
        cancelButton.setActionCommand("close");
        applyClosePanel.add(cancelButton, BorderLayout.CENTER);

        this.getContentPane().add(displayPanel, BorderLayout.CENTER);
        this.getContentPane().add(applyClosePanel, BorderLayout.SOUTH);

        pack();
        this.setResizable(false);
        setVisible(true);
    }

    /**
     * The actionPerformed method
     * 
     * @param event the ActionEvent
     * 
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equalsIgnoreCase("apply")) {
            checkedFileTypes = new ArrayList<String>();

            for (int i = 0; i < checkBoxArrList.size(); i++) {
                JCheckBox cBox = (JCheckBox) checkBoxArrList.get(i);
                if (cBox.isSelected()) {
                    checkedFileTypes.add(cBox.getText());
                }
            }

            StringBuffer sb = new StringBuffer();

            // add the checked box file types to the string buffer
            for (int i = 0; i < checkedFileTypes.size(); i++) {
                sb.append((String) checkedFileTypes.get(i));
                if (i != checkedFileTypes.size() - 1) {
                    sb.append("; ");
                }
            }

            // determine if user entered any file extensions in the user input area..if so...validate and if passed, add
            // on to string buffer
            if ( ! (userInput.getText().trim().equals(""))) {
                if ( !validateUserInputString(userInput.getText().trim())) {
                    MipavUtil.displayError("Additional file extensions must be unique and in the proper format. \n"
                            + "                                       ex. (*.abc;*.def)");
                    userInput.setText("");
                    return;
                } else {
                    if (checkedFileTypes.size() > 0) {
                        sb.append("; ");
                    }
                    for (int i = 0; i < userInputFileTypes.length; i++) {
                        sb.append(userInputFileTypes[i]);
                        if (i != userInputFileTypes.length - 1) {
                            sb.append("; ");
                        }
                    }
                }
            }

            // set the udef description
            if (checkedFileTypes.size() == 0 && userInputFileTypes == null) {
                ViewImageFileFilter.setUdefDescription("User Defined");
            } else {
                ViewImageFileFilter.setUdefDescription("User Defined (" + sb.toString() + ")");
            }


            // set the preferences
            Preferences.setProperty(Preferences.PREF_USER_FILETYPES, sb.toString());
            StringBuffer inputStringSB = new StringBuffer();
            if (userInputFileTypes != null) {
                for (int i = 0; i < userInputFileTypes.length; i++) {
                    inputStringSB.append(userInputFileTypes[i]);
                    if (i != userInputFileTypes.length - 1) {
                        inputStringSB.append(";");
                    }
                }
            }
            Preferences.setProperty(Preferences.PREF_USER_FILETYPES_TEXTFIELDS, inputStringSB.toString());

            //don't remember why i had this following code...the comment makes no sense....i commented out the 
            //whole thing...and everything seems to work
            
            // need to disasscociate and reset userdefinedFileTypesAssociations if user
            // has deleted a userDefined Extension that was part of that preferences
            /*if (userInput.getText().trim().equals("")) {
                Preferences.setProperty(Preferences.PREF_USER_FILETYPES, "");
            } else {
                if (Preferences.getProperty(Preferences.PREF_USER_FILETYPES) != null) {
                    if ( !Preferences.getProperty(Preferences.PREF_USER_FILETYPES).trim().equals("")) {
                        String[] associations = Preferences.getProperty(Preferences.PREF_USER_FILETYPES).split(";");
                        StringBuffer assocSB = new StringBuffer();
                        for (int i = 0; i < associations.length; i++) {
                            for (int k = 0; k < userInputFileTypes.length; k++) {
                                String uiFileType = userInputFileTypes[k].split("\\.")[1];
                                uiFileType = "." + uiFileType;
                                if (associations[i].split(":")[0].equals(uiFileType)) {

                                    if (sb.length() != 0) {
                                        assocSB.append(";");
                                    }
                                    assocSB.append(associations[i]);
                                    break;
                                }
                            }
                        }
                        System.out.println("assocSB is " + assocSB.toString());
                        Preferences.setProperty(Preferences.PREF_USER_FILETYPES, assocSB.toString());
                    }
                }
            }*/

            // set the cancel button text to 'close' since the changes were accepted
            cancelButton.setText("Close");
        } else if (command.equalsIgnoreCase("close")) { // close box
            dispose();   
        } else {
            super.actionPerformed(event);
        }

    }

    /**
     * This method renders the checkbox
     * 
     * @param checkBox the JCheckbox
     * @param gbLayout the GridbagLayout
     * @param gbConstraints the GridbagConstraints
     * @param x the x position
     * @param y the y position
     * 
     */
    public void renderCheckBox(JCheckBox checkBox, GridBagLayout gbLayout, GridBagConstraints gbConstraints, int x,
            int y) {
        gbConstraints.gridx = x;
        gbConstraints.gridy = y;
        gbConstraints.anchor = GridBagConstraints.WEST;
        gbConstraints.insets = new Insets(0, 5, 0, 5);
        gbLayout.setConstraints(checkBox, gbConstraints);
        displayPanel.add(checkBox);

    }

    /**
     * This method validates the user input file extensions. It alllows combinations of letters (upper and lowere) and
     * numbers in the file extension also makes sure that user input names do not duplicate any checkbox names
     * 
     * @param inputString the string to validate
     */
    public boolean validateUserInputString(String inputString) {
        userInputFileTypes = inputString.split(";");
        // take away duplicates in case user typed in duplicates
        LinkedHashSet<String> set = new LinkedHashSet<String>(Arrays.asList(userInputFileTypes));
        userInputFileTypes = set.toArray(new String[0]);

        for (int i = 0; i < userInputFileTypes.length; i++) {
            if ( ! (userInputFileTypes[i].trim().matches("\\*\\.\\w+?"))) {
                userInputFileTypes = null;
                return false;
            }
            for (int k = 0; k < checkboxNames.size(); k++) {
                if (userInputFileTypes[i].trim().equals((String) checkboxNames.get(k))) {
                    userInputFileTypes = null;
                    return false;
                }
            }
            // this is just in case they separate the file extensions with spaces
            userInputFileTypes[i] = userInputFileTypes[i].trim();
        }
        return true;
    }

    /**
     * This method does the check with the preferencesFileTypes array to determine if the checkbox should be checked
     * initalially
     * 
     * @param fileType the file extension to check
     */
    public boolean checkPreference(String fileType) {
        if (preferencesFileTypes != null) {
            for (int i = 0; i < preferencesFileTypes.length; i++) {
                if (fileType.equals(preferencesFileTypes[i])) {
                    return true;
                }
            }
            return false;
        } else {
            return false;
        }

    }

}
