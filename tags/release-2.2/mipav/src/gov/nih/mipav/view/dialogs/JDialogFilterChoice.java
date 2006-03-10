package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.view.*;
import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;

/**
*   Simple dialog to choose the filter for the view image
*   directory.
*
*   @author     Neva Cherniavsky
*   @version    1.0 June 1, 2002
*   @see        ViewImageFileFilter
*   @see        ViewImageDirectory
*/
public class JDialogFilterChoice extends JDialogBase {

    private ViewImageFileFilter imageFilter;
    private JCheckBox[]         checkImages;

    /**
    *   Initializes the dialog.
    *   @param parent   PArent frame of dialog.
    */
    public JDialogFilterChoice(Frame parent) {
        super(parent, true);
        init();
    }

    /**
    *   Initializes the dialog and adds the GUI components.
    */
    private void init() {
        setTitle("Choose Image Filter");

        String[] names = JDialogUnknownIO.getTypeNames();
        JPanel   panel = new JPanel(new GridLayout(names.length, 1));
    	panel.setForeground(Color.white);
    	panel.setBackground(Color.white);


        // set the filter type to the preferences saved filter
        int filter = 0;
        try {
            filter = Integer.parseInt(Preferences.getProperty("FilenameFilter"));
        }
        catch (NumberFormatException nfe) {
            // an invalid value was set in preferences -- so don't use it!
            filter = -1;
        }
        imageFilter = new ViewImageFileFilter(filter);

        checkImages = new JCheckBox[names.length];
        for (int i = 0; i < names.length; i++) {
            checkImages[i] = new JCheckBox(names[i]);
            checkImages[i].setFont(serif12);
            checkImages[i].setForeground(Color.black);
            checkImages[i].setBackground(Color.white);
            if (imageFilter.accept(JDialogUnknownIO.getSuffixFromIndex(i))) {
                checkImages[i].setSelected(true);
            }
            panel.add(checkImages[i]);
        }

        JScrollPane sp  = new JScrollPane(panel,
		                                  JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                          JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        JPanel mainPanel = new JPanel(new BorderLayout());
        mainPanel.add(sp);
        mainPanel.setBorder(buildTitledBorder("Choose types of images to display"));
        mainPanel.setPreferredSize(new Dimension(250, 400));

        JButton checkAll = new JButton("Select all");
        checkAll.setFont(serif12B);
        checkAll.setForeground(Color.black);
        checkAll.setPreferredSize(new Dimension(100, 30));
        checkAll.addActionListener(this);
        checkAll.setActionCommand("Check");

        JButton uncheckAll = new JButton("Clear");
        uncheckAll.setFont(serif12B);
        uncheckAll.setForeground(Color.black);
        uncheckAll.setPreferredSize(new Dimension(100, 30));
        uncheckAll.addActionListener(this);
        uncheckAll.setActionCommand("Uncheck");

        JPanel  checkPanel  = new JPanel();
        checkPanel.add(checkAll);
        checkPanel.add(uncheckAll);

        mainPanel.add(checkPanel, BorderLayout.SOUTH);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);

        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setVisible(true);
    }

    /**
    *   Sets the appropriate file filter when the "OK"
    *   button is pressed; otherwise disposes of the dialog.
    *   @param e    Event that triggered this function.
    */
    public void actionPerformed(ActionEvent e) {
        String command = e.getActionCommand();
        if (command.equals("OK")) {
            int count = 0;
            for (int i=0; i<checkImages.length; i++) {
                if (checkImages[i].isSelected()) {
                    count++;
                }
            }

            String[] exts = new String[count];
            count = 0;
            for (int i=0; i<checkImages.length; i++) {
                if (checkImages[i].isSelected()) {
                    exts[count++] = JDialogUnknownIO.getSuffixFromIndex(i);
                }
            }
            imageFilter = new ViewImageFileFilter(exts);
            dispose();
        }
        else if (command.equals("Cancel")) {
            cancelFlag = true;
            dispose();
        }
        else if (command.equals("Check")) {
            for (int i=0; i<checkImages.length; i++) checkImages[i].setSelected(true);
        }
        else if (command.equals("Uncheck")) {
            for (int i=0; i<checkImages.length; i++) checkImages[i].setSelected(false);
        }
    }

    /**
    *   Accessor that gets the file filter.
    *   @return The file filter.
    */
    public ViewImageFileFilter getFilter() { return imageFilter; }
}
