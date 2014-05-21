package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.InputStreamReader;
import java.net.URL;
import java.text.DecimalFormat;
import java.util.ArrayList;

public class StatisticsTable {
	
	/** t-statistics table */
	private static ArrayList<Double[]> tTable = null;
	
	static {
		if(tTable == null) {
			tTable = buildTinvTable();
		}
	}
	
	/**
	 * Finds the value of the two-tail t-statistic that produces the given level of significance for the given 
	 * number of degrees of freedom.  
	 * 
	 * @param dof degrees of freedom
	 * @param significance sig level
	 * @return t-statistic value
	 */
	public static Double getTwoTailInvTStatsitic(int dof, double significance) {
		DecimalFormat dec = new DecimalFormat("0.###");
		return getOneTailInvTStatsitic(dof, Double.valueOf(dec.format((1-(1-significance)/2))));
	}
	
	/**
	 * Finds the value of the one-tail t-statistic that produces the given level of significance for the given 
	 * number of degrees of freedom.  
	 * 
	 * @param dof degrees of freedom
	 * @param significance sig level
	 * @return t-statistic value
	 */
	public static Double getOneTailInvTStatsitic(int dof, double significance) {
		Double[] sigLevels = tTable.get(0); //first row gives significance levels
		int sigIndex = -1;
		for(int i=0; i<sigLevels.length; i++) {
			if(sigLevels[i] == significance) {
				sigIndex = i;
				break;
			}
		}
		
		if(sigIndex == -1) {
			Preferences.debug("Significance level of "+sigIndex+" does not exist in t-statistic table.", Preferences.DEBUG_MINOR);
			return null;
		}
		
		if(dof > 100) {
			dof = 101;
		}
		
		return tTable.get(dof)[sigIndex]; //subsequent rows are the dof
	}
	
	/**
	 * Builds the t-statistic table, first numerical row is list of significant values, subsequent rows
	 * are the t-statistic values for incremental degrees of freedom.
	 */
	private static ArrayList<Double[]> buildTinvTable() {
		BufferedReader in = null;
		try {
			URL obj = Thread.currentThread().getContextClassLoader().getResource("tDist.csv");
			in = new BufferedReader(new InputStreamReader(obj.openStream()));
		} catch (Exception e1) {
			MipavUtil.displayError("Unable to read statistics tables");
			e1.printStackTrace();
		}
		ArrayList<Double[]> ar = new ArrayList<Double[]>();
		boolean sigSet = false;
		int sigSize = -1;
		try {
			String line = new String();
			while((line = in.readLine()) != null) {
				if(!sigSet && line.contains("DOF")) {
					String[] sigString = line.split(",");
					sigSize = sigString.length - 1;
					Double[] sig = new Double[sigSize];
					for(int i=1; i<sigString.length; i++) {
						sig[i-1] = Double.valueOf(sigString[i]);
					}
					ar.add(sig);
					sigSet = true;
				} else if(sigSet) {
					Double[] val = new Double[sigSize];
					String[] valString = line.split(",");
					for(int i=1; i<valString.length; i++) {
						val[i-1] = Double.valueOf(valString[i]);
					}
					ar.add(val);
				}
			}
		} catch(Exception e) {
			MipavUtil.displayError("Unable to read statistics tables");
			e.printStackTrace();
		}
		
		return ar;
	}
}
