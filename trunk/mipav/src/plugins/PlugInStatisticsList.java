import gov.nih.mipav.view.JPanelStatisticsList;

interface ExtraPluginStatistics {
    String totalArea = "MuscleSeg Total Area";
    String totalFatArea = "MuscleSeg Fat Area";
    String totalLeanArea = "MuscleSeg Lean Area";
    String meanAreaTotalHU = "MuscleSeg Area Mean Total HU";
    String meanAreaFatHU = "MuscleSeg Area Mean Fat HU";
    String meanAreaLeanHU = "MuscleSeg Area Mean Lean HU";
    
    String totalVolume = "MuscleSeg Total Volume";
    String totalFatVolume = "MuscleSeg Fat Volume";
    String totalLeanVolume = "MuscleSeg Lean Volume";
    String meanVolumeTotalHU = "MuscleSeg Volume Mean Total HU";
    String meanVolumeFatHU = "MuscleSeg Volume Mean Fat HU";
    String meanVolumeLeanHU = "MuscleSeg Volume Mean Lean HU";
}

public class PlugInStatisticsList extends JPanelStatisticsList implements ExtraPluginStatistics {
    
    static String[] extendedStatisticsDescription;
    
    static {
        extendedStatisticsDescription = new String[statisticDescription.length+12];
        for(int i=0; i<statisticDescription.length; i++) {
            extendedStatisticsDescription[i] = statisticDescription[i];
        }
        extendedStatisticsDescription[statisticDescription.length] = totalArea;
        extendedStatisticsDescription[statisticDescription.length+1] = totalFatArea;
        extendedStatisticsDescription[statisticDescription.length+2] = totalLeanArea;
        extendedStatisticsDescription[statisticDescription.length+3] = meanAreaTotalHU;
        extendedStatisticsDescription[statisticDescription.length+4] = meanAreaFatHU;
        extendedStatisticsDescription[statisticDescription.length+5] = meanAreaLeanHU;
        
        extendedStatisticsDescription[statisticDescription.length+6] = totalVolume;
        extendedStatisticsDescription[statisticDescription.length+7] = totalFatVolume;
        extendedStatisticsDescription[statisticDescription.length+8] = totalLeanVolume;
        extendedStatisticsDescription[statisticDescription.length+9] = meanVolumeTotalHU;
        extendedStatisticsDescription[statisticDescription.length+10] = meanVolumeFatHU;
        extendedStatisticsDescription[statisticDescription.length+11] = meanVolumeLeanHU;
        
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