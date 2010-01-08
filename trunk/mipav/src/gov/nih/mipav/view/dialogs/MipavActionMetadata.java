package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.MipavUtil;

import java.util.*;


public abstract class MipavActionMetadata implements ActionMetadata {

    public String[] getAuthors() {
        return new String[] {"Matthew J. McAuliffe"};
    }

    public String[] getAffiliation() {
        return new String[] {"National Institutes of Health"};
    }

    public String[] getCitations() {
        // returning null indicates no citations according to Bennett
        return null;
    }

    public abstract String getCategory();

    public String getShortLabel() {
        return new String();
    }

    public abstract String getLabel();

    public abstract String getName();

    public String getDescription() {
        return new String();
    }

    public String getDescriptionLong() {
        return new String();
    }

    public String getVersion() {
        return MipavUtil.getVersion();
    }

    public String getWebsite() {
        return "http://mipav.cit.nih.gov/";
    }

    public Set<ImageRequirements> getInputImageRequirements() {
        return EnumSet.noneOf(ImageRequirements.class);
    }

    public String toString() {
        String str = "";

        str += "Name:\t\t\t" + getName() + "\n";
        str += "Category:\t\t" + getCategory() + "\n";
        str += "Description:\t\t" + getDescription() + "\n";
        str += "Description Long:\t" + getDescriptionLong() + "\n";
        str += "Short Label:\t\t" + getShortLabel() + "\n";
        str += "Label:\t\t\t" + getLabel() + "\n";
        str += "Version:\t\t" + getVersion() + "\n";
        str += "Website:\t\t" + getWebsite() + "\n";
        str += "Authors:\t\t" + getAuthorsString() + "\n";
        str += "Affiliation:\t\t" + getAffiliationString() + "\n";
        str += "Citations:\t\t" + getCitationsString() + "\n";
        str += "Input Image Reqs:\t" + getInputImageRequirementsString() + "\n";

        return str;
    }

    public String getAuthorsString() {
        String str = getAuthors()[0];
        for (int i = 1; i < getAuthors().length; i++) {
            str += ", " + getAuthors()[i];
        }
        return str;
    }

    public String getAffiliationString() {
        String str = getAffiliation()[0];
        for (int i = 1; i < getAffiliation().length; i++) {
            str += ", " + getAffiliation()[i];
        }
        return str;
    }

    public String getCitationsString() {
        if (getCitations() == null) {
            return "";
        }

        String str = getCitations()[0];
        for (int i = 1; i < getCitations().length; i++) {
            str += ", " + getCitations()[i];
        }
        return str;
    }

    public String getInputImageRequirementsString() {
        String str = "";
        final Set<ImageRequirements> reqs = getInputImageRequirements();
        for (final ImageRequirements r : reqs) {
            if (str.equals("")) {
                str += r.description;
            } else {
                str += ", " + r.description;
            }
        }
        return str;
    }
}
