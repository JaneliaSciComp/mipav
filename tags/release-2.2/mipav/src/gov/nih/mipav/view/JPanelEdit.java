package gov.nih.mipav.view;

import javax.swing.*;
import java.awt.*;


/**
*   An abstract class for editing a single, specfic value.
*   If the panel contains information which must be verified
*   before assembling as a panel value, then the owner must
*   checkFields() before requesting the panel's value.
*   @author David Parsons
*   @version 1.0
*/
public abstract class JPanelEdit extends JPanel {

    /** boolean specifying whether this panel needs to run its own
    *   error-check on itself, before the owner can get a panel value
    */
    // public to ensure that calling dialog class may know whether true or false
    //public boolean necessaryToCheckFields;

    /** A changable property name for firing <code>PropertyChangeEvent</code>s.
     *  A panel has but one unique property associated with it,
     *  that is the file it references, but in a situation where more than
     *  one such panel may be listened to by a
     *  <code>PropertyChangeListener</code>, each of these panels may be
     *  given a unique name to help the Listener.  Be careful when using this
     *  feature, however, because if there is more than one listener which is
     *  interested in changes in the <code>PropertyChangeEvent</code>,
     *  changing the property name <i>will</i> cause conflicts (and likely
     *  ignored events).
     *
     */
    protected String propertyName = "Editor Panel";

    /** when necessary To CheckFields, and there is a problem with the
    *   panel such that a valid panel value cannot be made, the
    *   problem will be reported in this String
    */
    protected String  errorString;

    /** when necessary To CheckFields, and there is a problem with the
    *   panel such that a valid panel value cannot be made,
    *   the particular component which has a bad value will be pointed
    *   to here.
    */
    protected Component errorComponent;

    /** abstract method returning the panel's value.
    *   @return The value of the Panel in some understandable String format
    */
    public abstract String getPanelValue();

    /** abstract method returning the panel's value.
    *   <p>
    *   If the information held by the panel is stored as a
    *   code (as in 'M' for "male" or 'F' for "female"), then
    *   this code will be returned by this method.  If there is
    *   no code for this value, this method will return a
    *   the same value as getPanelValue()
    *   @return the coded value.  If the panel <i>has</i> no coded
    *           value, then the return value will be the String
    *           returned by getPanelValue().
    */
    public abstract Object getCodedValue();

    /** Abstract method notifying the owner if the incorporated fields
    *   need to be re-entered.  Fields need to be re-entered when the
    *   panel cannot make a panelValue.
    *   see errorString
    *   see errorComponent
    *   see getPanelValue
    */
    public abstract boolean checkFields();      // returns fieldsOKAY;

    /*  Builds a standard JPanel.  It sets the check-fields to <code>false</code>
    *   @see JPanel
    */
    public JPanelEdit() {
        super();
    }

    /* * abstract method to invoke the JPanelEdit with initial text.
    *   JPanelEdits are to edit in a user-simplistic way DICOM strings
    *   without intervention from the parent dialog or frame.
    */
    //public abstract JPanelEdit(String initialText);

    /** method returns the String describing an error making the
    *   panel value.  If there is no error, this method returns null.
    */
    public String getErrorString() {return errorString;}

    /** method returns the Component (visual GUI interface) that has
    *   an error making the panel value in an attempt to allow the user
    *   the chance to correct the problem.
    *   If there is no error, this method returns null
    */
    public Component getErrorComponent() {return errorComponent;}

    /** Sets the propertyName to allow the <code>PropertyChangeEvent</code>
     *  to send out a unique name when <code>PropertyChangeEvent</code>s are
     *  fired.  This panel has but one unique property associated with it,
     *  that is the file it references, but in a situation where more than
     *  one such panel may be listened to by a
     *  <code>PropertyChangeListener</code>, each of these panels may be
     *  given a unique name to help the Listener.  Be careful when using this
     *  feature, however, because if there is more than one listener which is
     *  interested in changes in the <code>PropertyChangeEvent</code>,
     *  changing the property name <i>will</i> cause conflicts (and likely
     *  ignored events).
     *
     * @throws IllegalArgumentException if the <code>newPropertyName</code> is
     *          null or is empty.
     */
    public void setPropertyName(String newPropertyName)
        throws IllegalArgumentException
    {
        if (newPropertyName == null || newPropertyName.equals("")) {
            throw new IllegalArgumentException();
        }
        else { // ...set property Name
            propertyName = newPropertyName;
        }
    }

    /** Returns the current name used to identify
     *  <code>PropertyChangeEvent</code>s.
     */
    public String getPropertyName() {
        return propertyName;
    }

}

