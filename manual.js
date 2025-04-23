import React, { useState } from "react";
import styles from "../styles/manual.module.css";
import Footer from "../components/footer";
import Header from "../components/header";

const Manual = () => {
  const [activeTab, setActiveTab] = useState("Homepage");

  const tabs = [
    {
      name: "Homepage",
      content: (
        <div>
          <h2>Homepage Overview</h2>
          <p>
            The homepage serves as the central hub for the Black Pepper
            Knowledgebase, providing quick access to all resources and features.
          </p>

          <h3>Main Features:</h3>
          <ul className={styles.manual}>
            <li>
              <strong>Navigation Bar:</strong> Located at the top of every page,
              providing access to all major sections.
            </li>
            <li>
              <strong>Banner Section:</strong> Highlights the platform's purpose
              as a translational genomics resource.
            </li>
            <li>
              <strong>Introduction:</strong> Overview of PepperKB and its role
              in consolidating Piper nigrum data.
            </li>
            <li>
              <strong>Background Information:</strong> Interactive tab-based
              layout with sections on:
              <ul>
                <li>Classification (with color-coded taxonomy levels)</li>
                <li>Economic significance</li>
                <li>Cultivation regions</li>
                <li>Medicinal uses</li>
              </ul>
            </li>
            <li>
              <strong>Interactive Production Map:</strong> Displays global black
              pepper production trends (2000-2022).
            </li>
            <li>
              <strong>Tools & Databases Section:</strong> Card-based layout
              featuring eight specialized utilities.
            </li>
          </ul>

          <h3>Navigation Menu</h3>
          <p>
            The top navigation menu provides quick access to all major sections:
          </p>
          <ul className={styles.manual}>
            <li>
              <strong>Home:</strong> Returns to the main landing page.
            </li>
            <li>
              <strong>Browse:</strong> Access gene families, SNP markers, and
              other data collections.
            </li>
            <li>
              <strong>Tools:</strong> Access analytical tools including eFP
              Browser, BLAST, and JBrowse.
            </li>
            <li>
              <strong>Help:</strong> Documentation and user assistance
              resources.
            </li>
            <li>
              <strong>Contact:</strong> Communication portal for reaching the
              database team.
            </li>
          </ul>
          <img
            src="/images/navigationbar.png"
            alt="Top Navigation Bar"
            className={styles.image}
          />

          <h3>Quick Navigation</h3>
          <p>
            The quick links section provides direct access to frequently used
            tools and resources.
          </p>
          <img
            src="/images/quicklinks.png"
            alt="Quick Links"
            className={styles.image}
          />

          <h3>Gene Search</h3>
          <p>
            Enter a gene ID (e.g., Pn1.2085) in the search bar located in the
            header to access the comprehensive gene summary page. The gene
            details are organized in a four-tab format showing:
          </p>
          <ul className={styles.manual}>
            <li>Gene Details (default view)</li>
            <li>Expression Data</li>
            <li>Sequence Information</li>
            <li>Related Publications</li>
          </ul>
        </div>
      ),
    },
    {
      name: "GO-Pep",
      content: (
        <div>
          <h2>GO-Pep: Gene Ontology Annotation Tool</h2>
          <p>
            GO-Pep facilitates retrieval and exploration of Gene Ontology (GO)
            annotations for black pepper genes, providing systematic
            categorization into molecular functions, biological processes, and
            cellular components.
          </p>

          <h3>How to Use:</h3>
          <ol className={styles.manual}>
            <li>
              <strong>Input Gene IDs:</strong>
              <ul>
                <li>
                  Enter up to 5 gene IDs in the provided fields (e.g., Pn1.1,
                  Pn1.10)
                </li>
                <li>
                  Click "Try Example" to populate fields with sample gene IDs
                </li>
                <li>Use "Clear" to reset all input fields</li>
              </ul>
            </li>
            <li>
              <strong>Submit Your Query:</strong>
              <ul>
                <li>Click the "Submit" button to retrieve annotations</li>
                <li>
                  The system validates inputs and prevents empty submissions
                </li>
              </ul>
            </li>
            <li>
              <strong>Interpret Results:</strong>
              <ul>
                <li>
                  Results are color-coded by ontology category:
                  <ul>
                    <li>
                      <span style={{ color: "#4285F4" }}>
                        Molecular Function
                      </span>{" "}
                      (blue)
                    </li>
                    <li>
                      <span style={{ color: "#34A853" }}>
                        Biological Process
                      </span>{" "}
                      (green)
                    </li>
                    <li>
                      <span style={{ color: "#A142F4" }}>
                        Cellular Component
                      </span>{" "}
                      (purple)
                    </li>
                  </ul>
                </li>
                <li>
                  Each GO term is hyperlinked to the AmiGO database for detailed
                  definitions
                </li>
                <li>Results display each gene as a separate entry</li>
              </ul>
            </li>
            <li>
              <strong>Troubleshooting:</strong>
              <ul>
                <li>
                  If no annotations exist for a queried gene, the system will
                  display a notification
                </li>
                <li>Invalid gene IDs will trigger error messages</li>
                <li>
                  You can toggle between search and results views without losing
                  data
                </li>
              </ul>
            </li>
          </ol>
          <img
            src="/images/go-pep-interface.png"
            alt="GO-Pep Interface"
            className={styles.image}
          />
        </div>
      ),
    },
    {
      name: "GeneViz",
      content: (
        <div>
          <h2>GeneViz: Gene Structure Visualization</h2>
          <p>
            GeneViz provides an interactive web interface for visualizing
            exon-intron structures of black pepper genes, allowing researchers
            to examine gene architecture in detail.
          </p>

          <h3>How to Use:</h3>
          <ol className={styles.manual}>
            <li>
              <strong>Input Gene IDs:</strong>
              <ul>
                <li>
                  Enter gene IDs in the text area (comma-separated or line
                  breaks)
                </li>
                <li>You can input up to 5 gene IDs at once</li>
                <li>
                  Click "Try an Example" to load sample gene IDs for
                  demonstration
                </li>
              </ul>
            </li>
            <li>
              <strong>Generate Visualizations:</strong>
              <ul>
                <li>
                  Click "Submit" to query the database and generate gene
                  structures
                </li>
                <li>
                  The system validates inputs and prevents empty submissions
                </li>
              </ul>
            </li>
            <li>
              <strong>Interpret Results:</strong>
              <ul>
                <li>
                  Each gene structure displays with:
                  <ul>
                    <li>Gene identifier at the top</li>
                    <li>Exon/intron count information</li>
                    <li>Genomic coordinates</li>
                  </ul>
                </li>
                <li>Exons appear as green rectangular blocks</li>
                <li>Introns appear as gray connecting lines</li>
                <li>
                  All elements are positioned according to their actual genomic
                  coordinates
                </li>
              </ul>
            </li>
            <li>
              <strong>Troubleshooting:</strong>
              <ul>
                <li>
                  The system reports invalid IDs with appropriate error messages
                </li>
                <li>
                  If data is missing for a gene, a notification will appear
                </li>
                <li>Empty searches are prevented by input validation</li>
              </ul>
            </li>
          </ol>
          <img
            src="/images/geneviz-interface.png"
            alt="GeneViz Interface"
            className={styles.image}
          />
        </div>
      ),
    },
    {
      name: "PepperExp",
      content: (
        <div>
          <h2>PepperExp: Gene Expression Heatmap</h2>
          <p>
            PepperExp enables interactive exploration of gene expression
            patterns in Piper nigrum through dynamic heatmap visualizations,
            supporting tissue-specific filtering and detailed sample metadata.
          </p>

          <h3>How to Use:</h3>
          <ol className={styles.manual}>
            <li>
              <strong>Input Gene IDs:</strong>
              <ul>
                <li>
                  Enter gene IDs in the text area (comma-separated or
                  line-delimited)
                </li>
                <li>
                  Click "Try Example" to load a preloaded dataset
                  (Pn1.1040-Pn21.406)
                </li>
              </ul>
            </li>
            <li>
              <strong>Apply Filters:</strong>
              <ul>
                <li>
                  Select specific tissues to focus your analysis:
                  <ul>
                    <li>Fruit</li>
                    <li>Leaf</li>
                    <li>Root</li>
                    <li>Flower</li>
                    <li>Stem</li>
                  </ul>
                </li>
                <li>Choose a color scale for the heatmap visualization</li>
              </ul>
            </li>
            <li>
              <strong>Generate Visualization:</strong>
              <ul>
                <li>Click "Generate Heatmap" to process your query</li>
                <li>The system validates inputs and retrieves FPKM values</li>
              </ul>
            </li>
            <li>
              <strong>Interact with Results:</strong>
              <ul>
                <li>
                  The heatmap displays with:
                  <ul>
                    <li>Genes as rows</li>
                    <li>Experimental samples as columns</li>
                    <li>
                      Expression levels (FPKM) visualized via color intensity
                    </li>
                  </ul>
                </li>
                <li>Hover over cells to view exact FPKM values</li>
                <li>Use zoom/pan functionality for detailed inspection</li>
                <li>
                  View the sample metadata table below for experimental context
                </li>
              </ul>
            </li>
            <li>
              <strong>Sample Metadata:</strong>
              <ul>
                <li>
                  The table provides detailed experimental information:
                  <ul>
                    <li>Sample ID</li>
                    <li>Study accession codes</li>
                    <li>Sequencing platform</li>
                    <li>Experimental conditions</li>
                  </ul>
                </li>
                <li>
                  Use pagination controls and search function to navigate large
                  datasets
                </li>
              </ul>
            </li>
          </ol>
          <img
            src="/images/pepperexp-interface.png"
            alt="PepperExp Interface"
            className={styles.image}
          />
        </div>
      ),
    },
    {
      name: "PepperClust",
      content: (
        <div>
          <h2>PepperClust: Gene Expression Clustering</h2>
          <p>
            PepperClust enables interactive exploration of gene expression
            patterns through hierarchical cluster analysis, helping researchers
            identify co-expressed genes and expression patterns.
          </p>

          <h3>How to Use:</h3>
          <ol className={styles.manual}>
            <li>
              <strong>Input Gene IDs:</strong>
              <ul>
                <li>Enter gene IDs in the text area (comma-separated)</li>
                <li>
                  Click "Try Example" to load demonstration genes (Pn1.1 to
                  Pn1.1043)
                </li>
              </ul>
            </li>
            <li>
              <strong>Customize Visualization:</strong>
              <ul>
                <li>
                  Select color scheme from dropdown menu:
                  <ul>
                    <li>Blues</li>
                    <li>Greens</li>
                    <li>Purples</li>
                    <li>And other options</li>
                  </ul>
                </li>
                <li>
                  Choose visualization mode:
                  <ul>
                    <li>Heatmap with Dendrogram (default)</li>
                    <li>Dendrogram-only view</li>
                  </ul>
                </li>
              </ul>
            </li>
            <li>
              <strong>Generate Results:</strong>
              <ul>
                <li>Click "Generate Visualization" to process your query</li>
                <li>Reset button clears all parameters if needed</li>
              </ul>
            </li>
            <li>
              <strong>Interpret Results:</strong>
              <ul>
                <li>
                  In heatmap mode:
                  <ul>
                    <li>Genes appear as rows</li>
                    <li>Samples as columns</li>
                    <li>Expression levels shown via color gradient</li>
                    <li>
                      Integrated dendrogram shows hierarchical relationships
                    </li>
                  </ul>
                </li>
                <li>
                  In dendrogram-only view:
                  <ul>
                    <li>Focus on gene clustering patterns</li>
                    <li>
                      Simplified display for clearer relationship visualization
                    </li>
                  </ul>
                </li>
                <li>
                  Interactive features include:
                  <ul>
                    <li>Zooming</li>
                    <li>Panning</li>
                    <li>Hover tooltips showing exact expression values</li>
                  </ul>
                </li>
              </ul>
            </li>
            <li>
              <strong>Sample Metadata:</strong>
              <ul>
                <li>
                  Supplementary table provides detailed information about
                  samples:
                  <ul>
                    <li>Sequencing platform (Illumina)</li>
                    <li>Study accession codes</li>
                    <li>Experimental descriptions</li>
                  </ul>
                </li>
              </ul>
            </li>
          </ol>
          <img
            src="/images/pepperclust-interface.png"
            alt="PepperClust Interface"
            className={styles.image}
          />
        </div>
      ),
    },
    {
      name: "TF Families",
      content: (
        <div>
          <h2>TF Family Exploration</h2>
          <p>
            The Transcription Factor Family interface provides comprehensive
            access to transcription factor families within the Piper nigrum
            genome, with detailed gene structure visualization capabilities.
          </p>

          <h3>How to Use:</h3>
          <ol className={styles.manual}>
            <li>
              <strong>Browse Families:</strong>
              <ul>
                <li>
                  Two browsing modes available:
                  <ul>
                    <li>View all 62 families simultaneously</li>
                    <li>Filter families alphabetically using A-Z tabs</li>
                  </ul>
                </li>
                <li>Each family appears as an interactive card</li>
              </ul>
            </li>
            <li>
              <strong>View Family Members:</strong>
              <ul>
                <li>Click on any TF family to display its member genes</li>
                <li>
                  Detailed table shows:
                  <ul>
                    <li>Gene ID (clickable links)</li>
                    <li>Chromosome location (e.g., Pn1)</li>
                    <li>Precise genomic coordinates (e.g., 5021000-5024500)</li>
                  </ul>
                </li>
                <li>
                  Use the search function to filter genes by chromosome name
                </li>
              </ul>
            </li>
            <li>
              <strong>Examine Gene Structure:</strong>
              <ul>
                <li>
                  Click any gene ID in the table to visualize its structure
                </li>
                <li>
                  The interactive diagram shows:
                  <ul>
                    <li>Exons as green rectangular blocks</li>
                    <li>Introns as connecting gray lines</li>
                    <li>Complete exon-intron organization</li>
                  </ul>
                </li>
                <li>
                  This allows comparison of gene structures within and across
                  families
                </li>
              </ul>
            </li>
          </ol>
          <img
            src="/images/tf-family-interface.png"
            alt="TF Family Interface"
            className={styles.image}
          />
        </div>
      ),
    },
    {
      name: "SNP Database",
      content: (
        <div>
          <h2>SNP Marker Database</h2>
          <p>
            The SNP Marker Database provides an integrated platform for
            exploring genetic variation in Piper nigrum, offering comprehensive
            access to single nucleotide polymorphism data.
          </p>

          <h3>How to Use:</h3>
          <ol className={styles.manual}>
            <li>
              <strong>Select a Study:</strong>
              <ul>
                <li>
                  Begin by choosing from available research datasets:
                  <ul>
                    <li>2024 population structure analysis</li>
                    <li>2025 genome-wide SNP catalog</li>
                  </ul>
                </li>
                <li>
                  Click "View Details" to proceed to the analysis interface
                </li>
              </ul>
            </li>
            <li>
              <strong>Specify Search Parameters:</strong>
              <ul>
                <li>Select a chromosome (e.g., "Pn1") from the dropdown</li>
                <li>
                  Define position range by entering start and end coordinates
                </li>
                <li>
                  The system suggests valid ranges based on your chromosome
                  selection
                </li>
                <li>
                  Use the genome-wide heatmap to identify variant hotspots
                </li>
              </ul>
            </li>
            <li>
              <strong>Analyze Results:</strong>
              <ul>
                <li>
                  Results are presented in two complementary formats:
                  <ul>
                    <li>
                      <strong>Tabular view:</strong> Detailed listing of each
                      SNP with chromosome, position, reference allele, and
                      alternative allele
                    </li>
                    <li>
                      <strong>Interactive visualization:</strong> Graphical
                      representation of SNP distribution within your specified
                      range
                    </li>
                  </ul>
                </li>
                <li>Hover over data points to view additional details</li>
                <li>Click on specific points to refine your search</li>
              </ul>
            </li>
          </ol>
        </div>
      ),
    },
    {
      name: "JBrowse2",
      content: (
        <div>
          <h2>JBrowse2 Genome Browser</h2>
          <p>
            JBrowse2 provides an interactive visualization platform for
            exploring the black pepper reference genome, enabling researchers to
            navigate genomic features, annotations, and variants.
          </p>

          <h3>How to Use:</h3>
          <ol className={styles.manual}>
            <li>
              <strong>Select Genome Assembly:</strong>
              <ul>
                <li>
                  Use the dropdown menu to choose Piper_nigrum_genome (Pn1
                  version)
                </li>
                <li>
                  Select between two exploration methods:
                  <ul>
                    <li>
                      "Show all regions of assembly" for complete chromosome
                      overview
                    </li>
                    <li>
                      Search box for direct input of coordinates, gene IDs, or
                      feature types
                    </li>
                  </ul>
                </li>
              </ul>
            </li>
            <li>
              <strong>Navigate Genomic Visualization:</strong>
              <ul>
                <li>
                  Three synchronized tracks display:
                  <ul>
                    <li>
                      <strong>Reference sequence track:</strong> DNA sequence
                      with adjustable resolution
                    </li>
                    <li>
                      <strong>Annotation track:</strong> Gene models with exons
                      (colored boxes) connected by intron lines
                    </li>
                    <li>
                      <strong>Variant track:</strong> SNP and indel positions
                      relative to reference
                    </li>
                  </ul>
                </li>
                <li>
                  Use zoom controls to adjust detail level from chromosome-scale
                  to single-nucleotide views
                </li>
              </ul>
            </li>
            <li>
              <strong>Inspect Features:</strong>
              <ul>
                <li>
                  Click on any feature to open a detailed pop-up window showing:
                  <ul>
                    <li>Precise genomic coordinates</li>
                    <li>Feature type and length</li>
                    <li>Unique identifiers</li>
                    <li>Source information</li>
                    <li>Complete coding sequence in FASTA format</li>
                  </ul>
                </li>
                <li>
                  Feature table provides tabular overview of all annotated
                  elements in the viewed region
                </li>
                <li>
                  Use sorting and filtering capabilities to locate specific
                  features
                </li>
              </ul>
            </li>
          </ol>
        </div>
      ),
    },
    {
      name: "Gallery",
      content: (
        <div>
          <h2>Black Pepper Gallery</h2>
          <p>
            The Gallery interface provides an interactive platform for exploring
            high-quality images of Piper nigrum varieties and plant morphology,
            with detailed information about each specimen.
          </p>

          <h3>How to Use:</h3>
          <ol className={styles.manual}>
            <li>
              <strong>Browse Collection:</strong>
              <ul>
                <li>
                  Upon entering, an auto-rotating carousel displays featured
                  images
                </li>
                <li>
                  Images cycle every five seconds with smooth fade transitions
                </li>
                <li>Click "Explore Collection" to access the main gallery</li>
              </ul>
            </li>
            <li>
              <strong>Explore Main Gallery:</strong>
              <ul>
                <li>
                  Adaptive masonry grid layout displays 13 images in an
                  optimized arrangement
                </li>
                <li>
                  Collection includes:
                  <ul>
                    <li>
                      Three Sri Lankan black pepper varieties (DingiRala,
                      BootaweRala, KohukumbureRala)
                    </li>
                    <li>
                      Detailed photographs of fruits, inflorescences, and leaves
                    </li>
                  </ul>
                </li>
                <li>Hover over any image to display zoom icon</li>
              </ul>
            </li>
            <li>
              <strong>View Detailed Images:</strong>
              <ul>
                <li>Click on any thumbnail to open enhanced view</li>
                <li>
                  Two viewing modes available:
                  <ul>
                    <li>
                      <strong>Variety images:</strong> Split-panel interface
                      showing photograph alongside agronomic data
                    </li>
                    <li>
                      <strong>Plant part images:</strong> Full-screen mode for
                      detailed morphological examination
                    </li>
                  </ul>
                </li>
              </ul>
            </li>
            <li>
              <strong>Access Variety Data:</strong>
              <ul>
                <li>
                  For variety images, detailed information includes:
                  <ul>
                    <li>Parentage</li>
                    <li>Yield characteristics (2,245–2,724 g/vine/year)</li>
                    <li>Biochemical properties (5.6–6.3% piperin content)</li>
                  </ul>
                </li>
                <li>
                  Use navigation controls to browse through the collection
                </li>
                <li>Keyboard shortcuts available for accessibility</li>
              </ul>
            </li>
          </ol>
          <img
            src="/images/gallery-interface.png"
            alt="Gallery Interface"
            className={styles.image}
          />
        </div>
      ),
    },
    {
      name: "BLAST",
      content: (
        <div>
          <h2>BLAST: Basic Local Alignment Search Tool</h2>
          <p>
            The BLAST tool allows researchers to compare nucleotide or protein
            sequences to the black pepper sequence database, identifying regions
            of similarity between sequences.
          </p>

          <h3>How to Use:</h3>
          <ol className={styles.manual}>
            <li>
              <strong>Select BLAST Program:</strong>
              <ul>
                <li>
                  Choose from available BLAST programs:
                  <ul>
                    <li>
                      blastn - Search nucleotide database using nucleotide query
                    </li>
                    <li>
                      blastp - Search protein database using protein query
                    </li>
                    <li>
                      blastx - Search protein database using translated
                      nucleotide query
                    </li>
                    <li>
                      tblastn - Search translated nucleotide database using
                      protein query
                    </li>
                    <li>
                      tblastx - Search translated nucleotide database using
                      translated nucleotide query
                    </li>
                  </ul>
                </li>
              </ul>
            </li>
            <li>
              <strong>Enter Your Query:</strong>
              <ul>
                <li>Paste your sequence in FASTA format in the input box</li>
                <li>
                  Alternatively, upload a FASTA file using the file upload
                  option
                </li>
                <li>Example sequences are available for demonstration</li>
              </ul>
            </li>
            <li>
              <strong>Set Parameters:</strong>
              <ul>
                <li>
                  Adjust search sensitivity:
                  <ul>
                    <li>E-value threshold</li>
                    <li>Word size</li>
                    <li>Max target sequences</li>
                  </ul>
                </li>
                <li>
                  Select database to search against (genomic, CDS, protein)
                </li>
              </ul>
            </li>
            <li>
              <strong>Submit and View Results:</strong>
              <ul>
                <li>
                  Results display includes:
                  <ul>
                    <li>Graphical overview of alignments</li>
                    <li>Tabular hit list with scores and E-values</li>
                    <li>
                      Detailed alignments showing exact matches and mismatches
                    </li>
                  </ul>
                </li>
                <li>Download results in various formats (XML, tabular)</li>
                <li>
                  Click on hit identifiers to view corresponding gene
                  information
                </li>
              </ul>
            </li>
          </ol>
        </div>
      ),
    },
    {
      name: "Contact",
      content: (
        <div>
          <h2>Contact Form</h2>
          <p>
            The Contact Page provides an intuitive interface for communicating
            with the Black Pepper Knowledgebase team, with responsive design and
            validation features.
          </p>

          <h3>How to Use:</h3>
          <ol className={styles.manual}>
            <li>
              <strong>Complete the Form:</strong>
              <ul>
                <li>
                  Fill in all required fields:
                  <ul>
                    <li>First Name</li>
                    <li>Last Name</li>
                    <li>Email Address (requires valid email format)</li>
                    <li>Message Content</li>
                  </ul>
                </li>
                <li>
                  Fields feature dynamic visual cues including floating labels
                  and focus borders
                </li>
              </ul>
            </li>
            <li>
              <strong>Submit Your Message:</strong>
              <ul>
                <li>Click "Send Message" to submit your inquiry</li>
                <li>
                  The system performs client-side validation before submission
                </li>
                <li>A loading spinner appears during processing</li>
              </ul>
            </li>
            <li>
              <strong>Confirmation and Feedback:</strong>
              <ul>
                <li>
                  Successful submissions trigger a "Message Sent!" confirmation
                </li>
                <li>The form automatically resets after three seconds</li>
                <li>
                  If errors occur, entered data is preserved and error messages
                  appear
                </li>
              </ul>
            </li>
          </ol>
          <img
            src="/images/contact-interface.png"
            alt="Contact Interface"
            className={styles.image}
          />
        </div>
      ),
    },
  ];

  const handleTabClick = (tabName) => {
    setActiveTab(tabName);
  };

  return (
    <>
      <Header />
      <main>
        <div className={styles.container}>
          <div className={styles.tabList}>
            {tabs.map((tab) => (
              <button
                key={tab.name}
                className={`${styles.tabItem} ${
                  activeTab === tab.name ? styles.activeTab : ""
                }`}
                onClick={() => handleTabClick(tab.name)}
              >
                {tab.name}
              </button>
            ))}
          </div>
          <div className={styles.tabContent}>
            {tabs.find((tab) => tab.name === activeTab)?.content || (
              <p>Select a tab to view instructions</p>
            )}
          </div>
        </div>
      </main>
      <Footer />
    </>
  );
};

export default Manual;
