//MIPAV is freely available from http://mipav.cit.nih.gov

//THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, 
//EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES 
//OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND 
//NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT 
//HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
//WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
//FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE 
//OR OTHER DEALINGS IN THE SOFTWARE. 

/*****************************************************************
******************************************************************

The MIPAV application is intended for research use only.
This application has NOT been approved for ANY diagnostic use 
by the Food and Drug Administration. There is currently no 
approval process pending. 

This software may NOT be used for diagnostic purposes.

******************************************************************
******************************************************************/

import gov.nih.mipav.view.JPanelStatisticsList;

public class PlugInStatisticsList542a extends JPanelStatisticsList implements ExtraPluginStatistics542a {

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