# BioHEART Resilience Analysis Detailed Flowchart

This document contains a detailed flowchart for the BioHEART Resilience Package, showing the key processing steps, data transformations, and important parameters.

```mermaid
graph TB
      %% Data Flow and Processing Pipeline
      subgraph "1. Data Preparation"
          A[Raw Cohort Data] -->|prepare_cohort_data| B[Standardized Data]
          B --> C[Unit Conversion<br/>mmol/L to mg/dL]
          B --> D[Gender Validation]
          B --> E[Binary Validation]
          B --> F[Range Checks]
          B --> G[Ethnicity Mapping]
      end

      subgraph "2. Risk Score Calculation"
          B -->|calculate_risk_scores| H[Risk Scores]
          H --> I[FRS Score<br/>Age: 30-74]
          H --> J[ASCVD Score<br/>Age: 20-79]
          H --> K[MESA Score<br/>Age: 45-85]
          H --> L[SCORE2<br/>Expects mmol/L]
          G -->|ethnicity mappings| J
          G -->|ethnicity mappings| K
      end

      subgraph "3. Ensemble Scoring"
          H -->|ensemble_risk_score| M[Ensemble Score]
          M --> N[OrderNorm<br/>Transformation]
          N --> O[Standardization<br/>mean=0, sd=1]
          O --> P[Average Score]
          M --> Q[Missing Pattern<br/>Analysis]
      end

      subgraph "4. ZINB Modeling"
          B --> R[CACS Values]
          P --> S[fit_cacs_model]
          R --> S
          S --> T[Zero-Inflated<br/>Neg Binomial Model]
          T --> U[Count Component]
          T --> V[Zero Component]
      end

      subgraph "5. Percentile Calculation"
          T -->|calculate_cacs_percentiles| W[CACS Percentiles]
          W --> X[Predicted Distribution]
          W --> Y[Observed Position]
          Y --> Z[Calcium Vulnerability<br/>Score]
      end

      subgraph "6. Resilience Classification"
          Z -->|classify_resilience| AA[Classifications]
          AA --> AB[Resilient<br/>< 20th percentile]
          AA --> AC[Reference<br/>40th-60th percentile]
          AA --> AD[Susceptible<br/>> 80th percentile]
          AA --> AE[Other/Missing]
      end

      subgraph "7. Visualization"
          AA --> AF[plot_cacs_vs_risk]
          AA --> AG[plot_risk_distribution]
          Z --> AH[plot_percentile_distribution]
          T --> AI[plot_model_diagnostics]
      end

      %% Main Pipeline Orchestration
      B ==>|resilience_analysis| AJ[Complete Results]
      AJ --> AK[Model Object]
      AJ --> AL[Classifications]
      AJ --> AM[Diagnostic Plots]
      AJ --> AN[Summary Statistics]

      %% Utility Functions
      subgraph "Utility Functions"
          AO[convert_cholesterol_units]
          AP[standardize]
          AQ[validate_columns]
          AR[check_missing_values]
          AS[validate_binary]
          AT[validate_gender]
      end

      %% Style definitions
      classDef dataNode fill:#e1f5fe,stroke:#01579b,stroke-width:2px
      classDef processNode fill:#f3e5f5,stroke:#4a148c,stroke-width:2px
      classDef modelNode fill:#fff3e0,stroke:#e65100,stroke-width:2px
      classDef outputNode fill:#e8f5e9,stroke:#1b5e20,stroke-width:2px
      classDef utilNode fill:#fce4ec,stroke:#880e4f,stroke-width:2px

      class A,B,R dataNode
      class C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q processNode
      class S,T,U,V,W,X,Y,Z modelNode
      class AA,AB,AC,AD,AE,AF,AG,AH,AI,AJ,AK,AL,AM,AN outputNode
      class AO,AP,AQ,AR,AS,AT utilNode

```

## Key Features and Parameters

### Risk Score Specifications
- **FRS**: Ages 30-74, general population risk
- **ASCVD**: Ages 20-79, cholesterol 130-320 mg/dL, requires ethnicity
- **MESA**: Ages 45-85, multi-ethnic cohorts, optional family history
- **SCORE2**: Ages 40-75 optimal, expects mmol/L units, European guidelines

### Model Parameters
- **ZINB Scaling**: Default factor of 100 for numerical stability
- **Ensemble Minimum**: Default 1 valid score required per subject
- **Classification Thresholds**: 20th, 40th, 60th, 80th percentiles (customizable)

### Data Quality Features
- **ID Preservation**: Subject identifiers maintained throughout pipeline
- **Missing Data Handling**: Score-specific warnings and patterns analysis
- **Validation**: Comprehensive range checks and data quality assessment
- **Diagnostics**: Model fit statistics and distributional tests

This flowchart shows the essential workflow while maintaining focus on the key parameters and decision points that users need to understand.
