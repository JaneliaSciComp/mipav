/**
 * 
 */
package gov.nih.mipav.view.input.spacenav;

import gov.nih.mipav.view.renderer.WildMagic.GPURenderBase;

import java.awt.Component;
//import java.util.EventObject;
import java.awt.event.MouseEvent;

/**
 * @author Ben Bleiberg
 * I hope this works - This class is to test if passing a event object to the processNavEvent callback
 * will take care of the null pointer error when using the 3D mouse
 *
 */
public class SpaceNavigatorEvent extends MouseEvent {

	/**
	 * this is here because eclipse stated to so I used generate a UID option
	 */
	private static final long serialVersionUID = 3087686667525077579L;

	public SpaceNavigatorEvent(GPURenderBase source) {
//		super(source);
		super((Component)source.GetCanvas(), 0, 0, 1, 2, 3, 4, false);
		// TODO Auto-generated constructor stub
	}

}
