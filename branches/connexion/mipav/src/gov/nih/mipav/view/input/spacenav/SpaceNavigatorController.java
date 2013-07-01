package gov.nih.mipav.view.input.spacenav;
import java.util.ArrayList;

import gov.nih.mipav.view.Preferences;
import net.java.games.input.Component; 
import net.java.games.input.Controller; 
import net.java.games.input.ControllerEnvironment; 
import net.java.games.input.ControllerEvent;
import net.java.games.input.ControllerListener;

/** 
 * 
 * Allows for controlling a SpaceNavigator, could also be generalized for any joystick-type device.
 * 
 * Details for: SpaceNavigator, Stick, Unknown 
 * Components: (8) 
 * 0. Z Axis, z, relative, analog, 0.0 
 * 1. Y Axis, y, relative, analog, 0.0 
 * 2. X Axis, x, relative, analog, 0.0 
 * 3. Z Rotation, rz, relative, analog, 0.0 
 * 4. Y Rotation, ry, relative, analog, 0.0 
 * 5. X Rotation, rx, relative, analog, 0.0 
 * 6. Button 0, 0, absolute, digital, 0.0 
 * 7. Button 1, 1, absolute, digital, 0.0 
 * No Rumblers 
 * No subcontrollers
 * 
 *  @author justinsenseney
 */ 

public class SpaceNavigatorController{ 

	public static final int NUM_BUTTONS = 2; 
	
	/** Default button indices */
	public static final int ZAXIS = 0; 
	public static final int YAXIS = 1; 
	public static final int XAXIS = 2; 
	public static final int ZROTATION = 3; 
	public static final int YROTATION = 4; 
	public static final int XROTATION = 5; 
	public static final int BUTTON0 = 6; 
	public static final int BUTTON1 = 7; 
	
	/** values returned by the device during poll (may not be correct) */
	private static final float MAX_ROTATION = 0.5600f; 
	private static final float MAX_TRANSLATION = 0.6130f; 
	
	private static Controller controller; 
	
	static {
		ControllerEnvironment ce = ControllerEnvironment.getDefaultEnvironment();
		Controller[] cs = ce.getControllers(); 
		if (cs.length == 0) { 
			Preferences.debug("No controllers found", Preferences.DEBUG_MINOR);  
		} else {
			Preferences.debug("Num. controllers: " + cs.length, Preferences.DEBUG_MINOR);  
		}
		
		controller = findSpaceNavigator(cs); 
		if(controller != null) {
			Preferences.debug("Space Navigator controller: " + controller.getName() + ", " + controller.getType()); 
		}
//		ce.addControllerListener(new ControllerListener() {
//			
//			@Override
//			public void controllerAdded(ControllerEvent arg0) {
//				System.out.println("entered controllerAdded");
//				ControllerEnvironment ce = ControllerEnvironment.getDefaultEnvironment();
//				Controller[] cs = ce.getControllers(); 
//				if (cs.length == 0) { 
//					Preferences.debug("No controllers found", Preferences.DEBUG_MINOR);  
//				} else {
//					Preferences.debug("Num. controllers: " + cs.length, Preferences.DEBUG_MINOR);  
//				}
//				
//				controller = findSpaceNavigator(cs); 
//				if(controller != null) {
//					Preferences.debug("Space Navigator controller: " + controller.getName() + ", " + controller.getType()); 
//				}
//			}
//			
//			@Override
//			public void controllerRemoved(ControllerEvent arg0) {
//				SpaceNavigatorPoller.deregisterAllListeners();
//			}
//		});
	}
	
	private static final Component[] comps; 
	
	static {
		if(controller != null) {
			comps = controller.getComponents(); 
			if (comps.length == 0) { 
				Preferences.debug("No components found", Preferences.DEBUG_MINOR);  
			} else {
				Preferences.debug("Number components: " + comps.length, Preferences.DEBUG_MINOR); 
			}
		} else {
			comps = new Component[0];
		}
	}
	
	/** Actual button indices */
	public static final int xAxisIdx, yAxisIdx, zAxisIdx, rxAxisIdx, ryAxisIdx, rzAxisIdx; 
	
	static {
		// get the indices for the axes of the analog sticks: (x,y) and (z,rz) 
		xAxisIdx = findCompIndex(comps, Component.Identifier.Axis.X, "x"); 
		yAxisIdx = findCompIndex(comps, Component.Identifier.Axis.Y, "y"); 
		zAxisIdx = findCompIndex(comps, Component.Identifier.Axis.Z, "z");  
		
		rxAxisIdx = findCompIndex(comps, Component.Identifier.Axis.RX, "rx"); 
		ryAxisIdx = findCompIndex(comps, Component.Identifier.Axis.RY, "ry"); 
		rzAxisIdx = findCompIndex(comps, Component.Identifier.Axis.RZ, "rz"); 
	}
	
	private static final int buttonsIdx[]; 
	
	static {
		buttonsIdx = new int[NUM_BUTTONS]; 
		int numButtons = 0; 
		Component c; 
		
		for (int i = 0; i < comps.length; i++) { 
			c = comps[i]; 
			if (isButton(c)) { // deal with a button 
				if (numButtons == NUM_BUTTONS) {// already enough buttons 
					Preferences.debug("Found an extra button; index: " + i + ". Ignoring it", Preferences.DEBUG_MINOR); 
				} else { 
					buttonsIdx[numButtons] = i; // store button index 
					Preferences.debug("Found " + c.getName() + "; index: " + i, Preferences.DEBUG_MINOR); 
					numButtons++; 
				} 
			} 
		} 
		
		// fill empty spots in buttonsIdx[] with -1's 
		if (numButtons < NUM_BUTTONS) { 
			Preferences.debug("Too few buttons (" + numButtons + "); expecting " + NUM_BUTTONS, Preferences.DEBUG_MINOR); 
			while (numButtons < NUM_BUTTONS) { 
				buttonsIdx[numButtons] = -1; 
				numButtons++; 
			} 
		} 
	}
	
	public static boolean hasSpaceNavigator() {
		if(controller != null) {
			return true;
		}
		ControllerEnvironment ce = ControllerEnvironment.getDefaultEnvironment();
		Controller[] cs = ce.getControllers(); 
		controller = findSpaceNavigator(cs);
		return controller != null;
	}
	
	private static Controller findSpaceNavigator (Controller[] cs) { //Assumes SpaceNavigatorController.controller == null
		Controller.Type type; 
		int i = 0; 
		ArrayList<Integer> stickNums = new ArrayList<Integer>();
		while (i < cs.length) { 
			type = cs[i].getType(); 
			if ((type == Controller.Type.STICK)) {
				stickNums.add(i); 
			}
			i++; 
		} 
	
		if (stickNums.size() == 0) { 
			Preferences.debug("No space navigator found"); 
			return null;
		} else {
			Preferences.debug("Space navigator index: " + i); 
		}
		
		String[] nameCompares = new String[stickNums.size()];
		for(int j=0; j<nameCompares.length; j++) {
			nameCompares[j] = cs[stickNums.get(j)].getName().toLowerCase();

			if(nameCompares[j].contains("space") && nameCompares[j].contains("navigator")) {
				return cs[stickNums.get(j)]; //returns controller indicated by the given name
			}
		}
		
		
		return null;
	} 

	private static int findCompIndex (Component[] comps, Component.Identifier id, String nm) { 
		Component c; 
		for (int i = 0; i < comps.length; i++) { 
			c = comps[i]; 
			if ((c.getIdentifier() == id)) { 
				Preferences.debug("Found " + c.getName() + " at index: " + i, Preferences.DEBUG_MINOR); 
				return i; 
			} 
		} 
		
		Preferences.debug("No " + nm + " component found", Preferences.DEBUG_MINOR); 
		return -1; 
	} 
	
	/** 
	* Return true if the component is a digital/absolute button, and 
	* its identifier name ends with "Button" (i.e. the 
	* identifier class is Component.Identifier.Button). 
	*/ 
	private static boolean isButton (Component c) { 
		if (!c.isAnalog() && !c.isRelative()) { // digital and absolute 
			String className = c.getIdentifier().getClass().getName(); 
			// Preferences.debug(c.getName() + " identifier: " + className); 
			if (className.endsWith("Button")) {
				return true; 
			}
		} 
		return false; 
	} 
	
	/** 
	* Return all the buttons in a single array. Each button value is 
	* a boolean. 
	*/ 
	public static boolean[] getButtons () { 
		boolean[] buttons = new boolean[NUM_BUTTONS]; 
		float value; 
		for (int i = 0; i < NUM_BUTTONS; i++) { 
			value = comps[buttonsIdx[i]].getPollData(); 
			buttons[i] = ((value == 0.0f) ? false : true); 
		} 
		return buttons; 
	} // end of getButtons() 


	public static boolean isButtonPressed (int pos) 
		/* Return the button value (a boolean) for button number 'pos'. 
		pos is in the range 1-NUM_BUTTONS to match the game pad 
		button labels. 
		*/ { 
		if ((pos != NUM_BUTTONS)) { 
			Preferences.debug("Button position out of range (1-" + NUM_BUTTONS + "): " + pos); 
			return false; 
		} 
		
		if (buttonsIdx[pos - 1] == -1) {// no button found at that pos 
			return false; 
		}
		
		float value = comps[buttonsIdx[pos - 1]].getPollData(); 
		// array range is 0-NUM_BUTTONS-1 
		return value != 0.0f; 
	} // end of isButtonPressed() 
	
	public static void poll() { 
		if(!controller.poll()){
			SpaceNavigatorPoller.deregisterAllListeners();
		}
	} 
	
	
	/** 
	 * X Translation 
	 * 
	 * @return float value between .613 and -.613 
	 */ 
	public static float getTX () { 
		return comps[xAxisIdx].getPollData(); 
	} 
	
	/** 
	 * Y Translation 
	 * 
	 * @return float value between .613 and -.613  
	 */ 
	public static float getTY () { 
		return comps[yAxisIdx].getPollData(); 
	} 
	
	/** 
	 * Z Translation 
	 * 
	 * @return float value between .613 and -.613 
	 */ 
	public static float getTZ () { 
		return comps[zAxisIdx].getPollData(); 
	} 
	
	
	/** 
	 * X Rotation 
	 * 
	 * @return float value between .560 and -.560 
	 */ 
	public static float getRX () { 
		return comps[rxAxisIdx].getPollData(); 
	} 
	
	/** 
	 * Y Rotation 
	 * 
	 * @return float value between .560 and -.560 
	 */ 
	public static float getRY () { 
		return comps[ryAxisIdx].getPollData(); 
	} 
	
	/** 
	 * Z Rotation 
	 * 
	 * @return float value between .560 and -.560  
	 */ 
	public static float getRZ () { 
		return comps[rzAxisIdx].getPollData(); 
	} 
	
	public static boolean checkIfSpaceNavNeedsCalibration(){
		return SpaceNavigatorPoller.checkIfNeedCalibration();
	}

//	@Override
//	public void controllerAdded(ControllerEvent arg0) {
//		
//	}
//
//	@Override
//	public void controllerRemoved(ControllerEvent arg0) {
//		SpaceNavigatorPoller.deregisterAllListeners();
//		
//	}
} 