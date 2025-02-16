import streamlit as st
import numpy as np
import math

# Page configuration
st.set_page_config(
    page_title="Y90RS Calculator",
    page_icon="🏥",
    layout="wide",
    initial_sidebar_state="expanded"
)

# Custom CSS for better styling
st.markdown("""
    <style>
    .main {
        padding: 0rem 1rem;
    }
    .stButton>button {
        width: 100%;
    }
    .risk-box {
        padding: 1rem;
        border-radius: 0.5rem;
        margin: 0.5rem 0;
    }
    </style>
""", unsafe_allow_html=True)

def calculate_meld3(bilirubin, creatinine, inr, sodium, female=False):
    """Calculate MELD 3.0 score"""
    # Convert units
    bilirubin_mgdl = bilirubin / 17.1  # μmol/L to mg/dL
    creatinine_mgdl = creatinine / 88.4  # μmol/L to mg/dL
    
    # Apply bounds
    bilirubin_mgdl = max(1, bilirubin_mgdl)
    creatinine_mgdl = max(1, min(4, creatinine_mgdl))
    inr = max(1, inr)
    sodium = max(125, min(137, sodium))
    
    # MELD 3.0 formula
    meld3 = (1.33 * female) + \
            (0.047 * max(137 - sodium, 0)) + \
            (1.42 * math.log(bilirubin_mgdl)) + \
            (1.18 * math.log(inr)) + \
            (3.09 * math.log(creatinine_mgdl))
    
    return round(meld3)

def calculate_y90rs(tumor_size, tumor_volume, afp, portal_vein_status, 
                   shunt_fraction, meld3, albumin, alt_ast_ratio, nlr, ecog):
    """Calculate Y90RS score"""
    score = 0
    
    # 1. Tumor Burden Component (0-7 points)
    # Size & Volume (0-4)
    if tumor_size <= 3 and tumor_volume <= 100:
        score += 0
    elif (tumor_size <= 5 and tumor_volume <= 300) or \
         (tumor_size <= 3 and tumor_volume <= 300) or \
         (tumor_size <= 5 and tumor_volume <= 100):
        score += 1
    elif (tumor_size <= 8 and tumor_volume <= 500):
        score += 2
    else:
        score += 4
    
    # AFP (0-3)
    if afp <= 20:
        score += 0
    elif afp <= 400:
        score += 1
    elif afp <= 1000:
        score += 2
    else:
        score += 3
    
    # 2. Vascular Status (0-5 points)
    # Portal Vein Status (0-3)
    portal_vein_scores = {
        "No thrombosis": 0,
        "Bland thrombosis": 1,
        "Segmental tumor thrombosis": 2,
        "Main/Lobar tumor thrombosis": 3
    }
    score += portal_vein_scores[portal_vein_status]
    
    # Shunt Fraction (0-2)
    if shunt_fraction <= 5:
        score += 0
    elif shunt_fraction <= 10:
        score += 1
    else:
        score += 2
    
    # 3. Liver Function/Reserve (0-6 points)
    # MELD 3.0 (0-2)
    if meld3 <= 10:
        score += 0
    elif meld3 <= 14:
        score += 1
    else:
        score += 2
    
    # Albumin (0-2)
    if albumin >= 35:
        score += 0
    elif albumin >= 28:
        score += 1
    else:
        score += 2
    
    # ALT/AST ratio (0-2)
    if alt_ast_ratio <= 1.5:
        score += 0
    elif alt_ast_ratio <= 2.0:
        score += 1
    else:
        score += 2
    
    # 4. Inflammatory/Performance Status (0-4 points)
    # NLR (0-2)
    if nlr <= 2.5:
        score += 0
    elif nlr <= 4.0:
        score += 1
    else:
        score += 2
    
    # ECOG Status (0-2)
    if ecog <= 1:
        score += 0
    elif ecog == 2:
        score += 1
    else:
        score += 2
    
    return score

def get_recommendations(risk_category):
    """Get treatment recommendations based on risk category"""
    recommendations = {
        "Low Risk": {
            "Pre-treatment": [
                "Standard pre-treatment workup",
                "Consider single-session treatment",
                "Standard liver function assessment",
                "Optional multidisciplinary review"
            ],
            "Treatment": [
                "Target dose: 120-150 Gy",
                "Consider whole-lobe treatment if indicated",
                "Standard personalized dosimetry",
                "Single-session approach preferred",
                "Consider selective/superselective approach"
            ],
            "Monitoring": [
                "Follow-up imaging at 3 months",
                "Liver function tests every 4-6 weeks",
                "Consider AFP monitoring q3 months",
                "Standard toxicity monitoring"
            ]
        },
        "Intermediate Risk": {
            "Pre-treatment": [
                "Mandatory multidisciplinary review",
                "Detailed vascular mapping",
                "Consider advanced liver function testing",
                "Assess portal vein flow dynamics",
                "Consider pre-treatment portal vein embolization"
            ],
            "Treatment": [
                "Target dose: 100-120 Gy",
                "Sequential lobar treatment recommended",
                "Consider radiation segmentectomy for small lesions",
                "Personalized dosimetry mandatory",
                "Consider prophylactic antibiotics"
            ],
            "Monitoring": [
                "Early follow-up imaging (6-8 weeks)",
                "Liver function tests every 2-3 weeks",
                "Monthly AFP monitoring",
                "Enhanced toxicity monitoring",
                "Consider admission for first treatment"
            ]
        },
        "High Risk": {
            "Pre-treatment": [
                "Extensive pre-treatment evaluation",
                "Full performance status assessment",
                "Detailed quality of life assessment",
                "Consider alternative treatments",
                "Mandatory portal pressure assessment",
                "Detailed nutritional assessment"
            ],
            "Treatment": [
                "Target dose: 80-100 Gy",
                "Selective/superselective approach mandatory",
                "Sequential treatment with 4-6 week interval",
                "Consider dose reduction",
                "Prophylactic antibiotics mandatory",
                "Consider systemic therapy combination"
            ],
            "Monitoring": [
                "Weekly monitoring first month",
                "Imaging at 4-6 weeks",
                "Biweekly liver function tests",
                "Consider hospital admission",
                "Enhanced toxicity monitoring",
                "Early palliative care consultation"
            ]
        }
    }
    return recommendations[risk_category]

# Main app
def main():
    st.title("Y90RS: Y90 Radioembolization Score Calculator")
    st.markdown("---")

    # Create two columns for input
    col1, col2 = st.columns(2)

    with col1:
        st.subheader("Tumor Burden Component")
        tumor_size = st.number_input("Largest tumor size (cm)", 0.0, 30.0, step=0.1)
        tumor_volume = st.number_input("Tumor volume (cc)", 0.0, 5000.0, step=0.1)
        afp = st.number_input("AFP (ng/mL)", 0.0, 1000000.0, step=0.1)
        
        st.subheader("Vascular Status")
        portal_vein_status = st.selectbox(
            "Portal vein status",
            ["No thrombosis", "Bland thrombosis", "Segmental tumor thrombosis", "Main/Lobar tumor thrombosis"]
        )
        shunt_fraction = st.number_input("Shunt fraction (%)", 0.0, 100.0, step=0.1)

    with col2:
        st.subheader("Liver Function/Reserve")
        meld3_input = st.radio("MELD 3.0 input method", ["Enter MELD 3.0 directly", "Calculate MELD 3.0"])
        
        if meld3_input == "Enter MELD 3.0 directly":
            meld3 = st.number_input("MELD 3.0 score", 0, 40)
        else:
            st.markdown("##### MELD 3.0 Calculator")
            bilirubin = st.number_input("Total Bilirubin (μmol/L)", 0.0, 1000.0, step=0.1)
            creatinine = st.number_input("Creatinine (μmol/L)", 0.0, 1000.0, step=0.1)
            inr = st.number_input("INR", 0.0, 10.0, step=0.1)
            sodium = st.number_input("Sodium (mmol/L)", 115.0, 160.0, step=0.1)
            female = st.checkbox("Female gender")
            
            if all(v > 0 for v in [bilirubin, creatinine, inr, sodium]):
                meld3 = calculate_meld3(bilirubin, creatinine, inr, sodium, female)
                st.info(f"Calculated MELD 3.0 score: {meld3}")
            else:
                meld3 = 0
        
        albumin = st.number_input("Albumin (g/L)", 0.0, 60.0, step=0.1)
        alt_ast_ratio = st.number_input("ALT/AST ratio", 0.0, 10.0, step=0.1)
        
        st.subheader("Inflammatory/Performance Status")
        nlr = st.number_input("NLR", 0.0, 50.0, step=0.1)
        ecog = st.selectbox("ECOG Performance Status", [0, 1, 2, 3])

    # Calculate button
    if st.button("Calculate Y90RS Score", type="primary"):
        score = calculate_y90rs(
            tumor_size, tumor_volume, afp, portal_vein_status,
            shunt_fraction, meld3, albumin, alt_ast_ratio, nlr, ecog
        )
        
        # Determine risk category
        if score <= 6:
            risk_category = "Low Risk"
            color = "success"
            mortality = "<10%"
        elif score <= 12:
            risk_category = "Intermediate Risk"
            color = "warning"
            mortality = "10-30%"
        else:
            risk_category = "High Risk"
            color = "error"
            mortality = ">30%"

        # Display results
        st.markdown("---")
        st.subheader("Results")
        
        # Create three columns for the results
        res_col1, res_col2, res_col3 = st.columns(3)
        
        with res_col1:
            st.metric("Total Score", f"{score}")
        with res_col2:
            st.metric("Risk Category", risk_category)
        with res_col3:
            st.metric("Mortality Risk", mortality)

        # Get recommendations
        recommendations = get_recommendations(risk_category)
        
        # Display recommendations
        st.markdown("### Management Recommendations")
        rec_col1, rec_col2, rec_col3 = st.columns(3)
        
        with rec_col1:
            st.markdown("#### Pre-treatment")
            for rec in recommendations["Pre-treatment"]:
                st.markdown(f"- {rec}")
        
        with rec_col2:
            st.markdown("#### Treatment")
            for rec in recommendations["Treatment"]:
                st.markdown(f"- {rec}")
        
        with rec_col3:
            st.markdown("#### Monitoring")
            for rec in recommendations["Monitoring"]:
                st.markdown(f"- {rec}")

if __name__ == "__main__":
    main()
