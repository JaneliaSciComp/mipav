package gov.nih.mipav.view.renderer.WildMagic.Interface;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.*;
import gov.nih.mipav.view.dialogs.JDialogAnnotation;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.plaf.basic.BasicBorders;


public class JDialogVolumeAnnotation extends JDialogAnnotation implements ActionListener {

	private static final long serialVersionUID = 6726637420209273103L;
	private JPanelAnnotationAnimation parentPanel;

	public JDialogVolumeAnnotation(ModelImage image, VOI textVOI, int element, boolean isRegistered,
			boolean modal, boolean editText, JPanelAnnotationAnimation parent ) {
        super(image, textVOI, element, isRegistered, modal, editText);
        this.parentPanel = parent;
    }


    /**
     * makes sure the dialog is ready after (OKAY) is pressed.
     *
     * @return  boolean are the dialog's variables kosher
     */
    protected boolean setVariables() {
    	super.setVariables();
    	VOIText text = getTextVOI();
    	if ( text != null )
    	{
    		text.updateText();
    	}
    	if ( parentPanel != null )
    	{
    		parentPanel.updateFonts( text );
    	}
    	return true;
    }

}
