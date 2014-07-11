package gov.nih.mipav.view.components;


import java.awt.*;

import javax.swing.*;


/**
 * Manages the adding of components to a panel, automatically tranlating between logical element placement and the
 * panel's layout.
 *
 * @author  mccreedy
 */
public class PanelManager {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private GridBagConstraints gbc;

    /** DOCUMENT ME! */
    private JPanel managedPanel;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Construct the panel manager with a new panel.
     */
    public PanelManager() {
        managedPanel = initPanel();
        gbc = initConstraints();
    }

    /**
     * Construct the panel manager with a new panel that has a titled border.
     *
     * @param  title  the title to put on the new panel's border
     */
    public PanelManager(String title) {
        this();
        managedPanel.setBorder(WidgetFactory.buildTitledBorder(title));
    }

    /**
     * Construct the panel manager and attach it to a panel which has already been created.
     *
     * @param  panel  the panel to manage
     */
    public PanelManager(JPanel panel) {
        setPanel(panel);
        gbc = initConstraints();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Adds a new component to the panel.
     *
     * @param  component  the component to add
     */
    public void add(JComponent component) {

        if (gbc.gridx == 0) {
            add(component, GridBagConstraints.WEST);
        } else {
            add(component, GridBagConstraints.EAST);
        }
    }

    /**
     * Adds a new component to the panel.
     *
     * @param  component       the component to add
     * @param  anchorPosition  the position to anchor the component to (from GridBagConstraints)
     */
    public void add(JComponent component, int anchorPosition) {
        managedPanel.add(component, gbc);

        // get ready to place the next component
        gbc.gridx++;
    }

    /**
     * Adds a new component to the panel on a new line (vertically).
     *
     * @param  component  the component to add
     */
    public void addOnNextLine(JComponent component) {
        moveToNextLine();
        add(component);
    }

    /**
     * Adds a new component to the panel on a new line (vertically).
     *
     * @param  component       the component to add
     * @param  anchorPosition  the position to anchor the component to (from GridBagConstraints)
     */
    public void addOnNextLine(JComponent component, int anchorPosition) {
        moveToNextLine();
        add(component, anchorPosition);
    }

    /**
     * Returns the current constraints of this panel manager.
     *
     * @return  the current grid bag constraints
     */
    public GridBagConstraints getConstraints() {
        return gbc;
    }

    /**
     * Returns the managed panel.
     *
     * @return  the managed panel
     */
    public JPanel getPanel() {
        return managedPanel;
    }

    /**
     * Changes the panel which is managed by this class. Forces the panel to use a grid bag layout.
     *
     * @param  panel  the panel to manage
     */
    public void setPanel(JPanel panel) {
        managedPanel = panel;
        panel.setLayout(new GridBagLayout());
    }

    /**
     * Create a new set of grid bag constraints.
     *
     * @return  new layout constraints
     */
    private GridBagConstraints initConstraints() {
        GridBagConstraints constraints = new GridBagConstraints();
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 1;
        constraints.fill = GridBagConstraints.HORIZONTAL;

        return constraints;
    }

    /**
     * Create a new panel.
     *
     * @return  a new panel, with a grid bag layout
     */
    private JPanel initPanel() {
        JPanel panel = new JPanel(new GridBagLayout());

        return panel;
    }

    /**
     * Force new components which are added to appear on the next line in the panel (vertically).
     */
    private void moveToNextLine() {
        gbc.gridx = 0;
        gbc.gridy++;
    }
}
