package gov.nih.mipav.view.input.spacenav;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Arrays;
import javax.swing.Timer;

/**
 * Polls the space navigator.
 * 
 * @author justinsenseney
 *
 */
public class SpaceNavigatorPoller {

	private static final int DELAY = 50;
	private static Timer pollTimer; 
	private static SpaceNavigatorListener[] listeners = new SpaceNavigatorListener[0];
	
	public static void fireSpaceNavEvent() {
		for(int i=0; i<listeners.length; i++) {
			listeners[i].processSpaceNavEvent();
		}
	}
	
	public static void deRegisterListener(SpaceNavigatorListener listener) {
		int toRemove = -1;
		for(int i=0; i<listeners.length; i++) {
			if(listeners[i] == listener) {
				toRemove = i;
				break;
			}
		}
		
		if(toRemove != -1) {
			SpaceNavigatorListener[] listenersNew = new SpaceNavigatorListener[listeners.length-1];
			for(int i=0; i<toRemove; i++) {
				listenersNew[i] = listeners[i];
			}
			
			for(int i=toRemove+1; i<listeners.length; i++) {
				listenersNew[i-1] = listeners[i]; 
			}
			
			listeners = listenersNew;
		}
		
		if(listeners.length == 0 && pollTimer != null) {
			pollTimer.stop();                                               
		}
	}
	
	public static void registerListener(SpaceNavigatorListener listener) {
		SpaceNavigatorListener[] listenersNew = Arrays.copyOf(listeners, listeners.length+1);
		listenersNew[listeners.length] = listener;
		listeners = listenersNew;
		
		if(pollTimer == null) {
			ActionListener pollPerformer = new ActionListener() { 
				public void actionPerformed (ActionEvent e) { 
					SpaceNavigatorController.poll();
					if(SpaceNavigatorController.getRX() != 0 || 
							SpaceNavigatorController.getRY() != 0 || 
							SpaceNavigatorController.getRZ() != 0 || 
							SpaceNavigatorController.getTX() != 0 || 
							SpaceNavigatorController.getTY() != 0 || 
							SpaceNavigatorController.getTZ() != 0) {
						fireSpaceNavEvent();
					}
				} 
			}; 
			pollTimer = new Timer(DELAY, pollPerformer); 
		}
		
		if(pollTimer != null && !pollTimer.isRunning()) {
			pollTimer.start(); 
		}
	}
	
	public static SpaceNavigatorListener[] getListeners() {
		return listeners;
	}
	
	public static boolean hasInstanceOf(SpaceNavigatorListener temp){
		for(int i=0; i<listeners.length; i++){
			if(listeners[i].equals(temp))
				return true;
		}
		
		return false;
	}
	
	public static boolean checkIfNeedCalibration(){
		SpaceNavigatorController.poll();
		if(SpaceNavigatorController.getRX() != 0 || 
				SpaceNavigatorController.getRY() != 0 || 
				SpaceNavigatorController.getRZ() != 0 || 
				SpaceNavigatorController.getTX() != 0 || 
				SpaceNavigatorController.getTY() != 0 || 
				SpaceNavigatorController.getTZ() != 0) {
			return true;
		}
		return false;
	}
	
	public static void deregisterAllListeners(){
		for(int i = 0; i < listeners.length; i++){
			deRegisterListener(listeners[i]);
		}
	}
}
