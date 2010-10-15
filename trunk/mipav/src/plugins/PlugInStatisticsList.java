import gov.nih.mipav.view.JPanelStatisticsList;

interface ExtraPluginStatistics {
    String totalArea = "Plugin Total Area";
    String totalFatArea = "Plugin Fat Area";
    String totalLeanArea = "Plugin Lean Area";
    String meanTotalHU = "Plugin Mean Total HU";
    String meanFatHU = "Plugin Mean Fat HU";
    String meanLeanHU = "Plugin Mean Lean HU";
}

public class PlugInStatisticsList extends JPanelStatisticsList implements ExtraPluginStatistics {
    
    static String[] extendedStatisticsDescription;
    
    static {
        extendedStatisticsDescription = new String[statisticDescription.length+6];
        for(int i=0; i<statisticDescription.length; i++) {
            extendedStatisticsDescription[i] = statisticDescription[i];
        }
        extendedStatisticsDescription[statisticDescription.length] = totalArea;
        extendedStatisticsDescription[statisticDescription.length+1] = totalFatArea;
        extendedStatisticsDescription[statisticDescription.length+2] = totalLeanArea;
        extendedStatisticsDescription[statisticDescription.length+3] = meanTotalHU;
        extendedStatisticsDescription[statisticDescription.length+4] = meanFatHU;
        extendedStatisticsDescription[statisticDescription.length+5] = meanLeanHU;
    }
    
    /**
     * Creates the list of labels to use in the checkboxes.
     *
     * @return  DOCUMENT ME!
     */
    public String[] makeCheckboxLabels() {
        return extendedStatisticsDescription;
    }

    public static String[] getCheckboxLabels() {
        return extendedStatisticsDescription;
    }
    
    protected void setListLength() {
        listLength = extendedStatisticsDescription.length;
    }
        
}