import React, { useState } from "react";
import Head from "next/head";
import styles from "../styles/go.module.css"; 
import Header from "../components/header";
import Footer from "../components/footer";

const MAX_GENES = 5; // Limiting to max 5 genes

const PepperGOPage = () => {
  const [geneInputs, setGeneInputs] = useState([""]); // Start with one input field
  const [goAnnotations, setGoAnnotations] = useState([]);
  const [error, setError] = useState("");
  const [invalidGeneIds, setInvalidGeneIds] = useState([]);
  const [isLoading, setIsLoading] = useState(false);
  const [activeTab, setActiveTab] = useState("input"); // 'input' or 'results'

  const fetchGOData = async (ids) => {
    setIsLoading(true);
    try {
      // Make API call to fetch GO annotation data
      const response = await fetch("/api/go-annotations-by-id", {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify({ geneIds: ids }),
      });

      if (!response.ok) {
        throw new Error(`API error: ${response.status}`);
      }

      const fetchedAnnotations = await response.json();

      // Check which IDs were not found
      const fetchedIds = fetchedAnnotations.map((item) =>
        item.GeneID.toUpperCase()
      );
      const notFoundIds = ids.filter(
        (id) => !fetchedIds.includes(id.toUpperCase())
      );

      setInvalidGeneIds(notFoundIds);
      setGoAnnotations(fetchedAnnotations);

      if (fetchedAnnotations.length === 0) {
        setError("No GO annotations found for the provided GeneIDs.");
      } else {
        setError("");
        setActiveTab("results"); // Switch to results tab
      }
    } catch (err) {
      console.error("Error fetching GO annotation data:", err);
      setError("Failed to fetch GO annotations. Please try again.");
    } finally {
      setIsLoading(false);
    }
  };

  const handleSubmit = (e) => {
    e.preventDefault();
    setError("");
    setInvalidGeneIds([]);

    // Filter out empty inputs
    const validInputs = geneInputs.filter((input) => input.trim() !== "");
    const ids = validInputs.map((input) => input.trim().toUpperCase());

    if (ids.length === 0) {
      setError("Please enter at least one GeneID.");
      return;
    }

    fetchGOData(ids);
  };

  const handleInputChange = (index, value) => {
    const updatedInputs = [...geneInputs];
    updatedInputs[index] = value;
    setGeneInputs(updatedInputs);
  };

  const addInputField = () => {
    if (geneInputs.length < MAX_GENES) {
      setGeneInputs([...geneInputs, ""]);
    }
  };

  const removeInputField = (index) => {
    const updatedInputs = geneInputs.filter((_, i) => i !== index);
    setGeneInputs(updatedInputs);
  };

  const handleClear = () => {
    setGeneInputs([""]);
    setGoAnnotations([]);
    setError("");
    setInvalidGeneIds([]);
    setActiveTab("input");
  };

  const handleExample = () => {
    setGeneInputs(["Pn1.1", "Pn1.10"]);
  };

  // Function to get category full name
  const getCategoryName = (code) => {
    switch (code) {
      case "F":
        return "Molecular Function";
      case "P":
        return "Biological Process";
      case "C":
        return "Cellular Component";
      default:
        return code;
    }
  };

  // Get category icon
  const getCategoryIcon = (code) => {
    switch (code) {
      case "F":
        return "fas fa-cogs";
      case "P":
        return "fas fa-dna";
      case "C":
        return "fas fa-layer-group";
      default:
        return "fas fa-question";
    }
  };

  // Get category color
  const getCategoryColor = (code) => {
    switch (code) {
      case "F":
        return styles.molecularFunction;
      case "P":
        return styles.biologicalProcess;
      case "C":
        return styles.cellularComponent;
      default:
        return "";
    }
  };

  return (
    <>
      <Head>
        <title>PepperGO - PepperKB</title>
        <link
          rel="stylesheet"
          href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css"
        />
      </Head>

      <Header />
      <main className={styles.appContainer}>
        <div className={styles.heroSection}>
          <h1 className={styles.appTitle}>
            {/* <span className={styles.appIcon}>🧬</span> */}
            GO-Pep
          </h1>
          <p className={styles.appDescription}>
            Explore Gene Ontology (GO) annotations for pepper genes
          </p>
        </div>

        <div className={styles.contentContainer}>
          {/* Navigation tabs */}
          <div className={styles.tabNav}>
            <button
              className={`${styles.tabButton} ${
                activeTab === "input" ? styles.activeTab : ""
              }`}
              onClick={() => setActiveTab("input")}
            >
              <i className="fas fa-search"></i> Search
            </button>
            <button
              className={`${styles.tabButton} ${
                activeTab === "results" ? styles.activeTab : ""
              }`}
              onClick={() =>
                goAnnotations.length > 0 && setActiveTab("results")
              }
              disabled={goAnnotations.length === 0}
            >
              <i className="fas fa-list-alt"></i> Results
              {goAnnotations.length > 0 && (
                <span className={styles.resultCount}>
                  {goAnnotations.length}
                </span>
              )}
            </button>
          </div>

          {/* Input Panel */}
          {activeTab === "input" && (
            <div className={styles.card}>
              <div className={styles.cardHeader}>
                <h2>Search Gene Ontology</h2>
                <button
                  onClick={handleExample}
                  className={styles.exampleButton}
                >
                  <i className="fas fa-lightbulb"></i> Try Example
                </button>
              </div>

              <div className={styles.infoBox}>
                <i className="fas fa-info-circle"></i>
                <span>
                  Enter up to {MAX_GENES} gene IDs to explore their GO
                  annotations
                </span>
              </div>

              <form onSubmit={handleSubmit}>
                <div className={styles.inputList}>
                  {geneInputs.map((input, index) => (
                    <div key={index} className={styles.inputRow}>
                      <div className={styles.inputField}>
                        <input
                          type="text"
                          value={input}
                          onChange={(e) =>
                            handleInputChange(index, e.target.value)
                          }
                          placeholder={`Gene ID ${index + 1} (e.g., Pn1.1)`}
                          className={styles.textInput}
                        />
                      </div>
                      {index > 0 && (
                        <button
                          type="button"
                          onClick={() => removeInputField(index)}
                          className={styles.removeButton}
                          aria-label="Remove gene input"
                        >
                          <i className="fas fa-times"></i>
                        </button>
                      )}
                    </div>
                  ))}
                </div>

                {geneInputs.length < MAX_GENES && (
                  <button
                    type="button"
                    onClick={addInputField}
                    className={styles.addButton}
                  >
                    <i className="fas fa-plus"></i> Add Gene
                  </button>
                )}

                <div className={styles.formActions}>
                  <button
                    type="submit"
                    className={styles.primaryButton}
                    disabled={isLoading}
                  >
                    {isLoading ? (
                      <>
                        <i className="fas fa-circle-notch fa-spin"></i>{" "}
                        Searching...
                      </>
                    ) : (
                      <>
                        <i className="fas fa-search"></i> Find Annotations
                      </>
                    )}
                  </button>
                  <button
                    type="button"
                    className={styles.secondaryButton}
                    onClick={handleClear}
                  >
                    <i className="fas fa-eraser"></i> Clear
                  </button>
                </div>
              </form>

              {error && (
                <div className={styles.errorAlert}>
                  <i className="fas fa-exclamation-circle"></i>
                  {error}
                </div>
              )}

              {invalidGeneIds.length > 0 && (
                <div className={styles.warningAlert}>
                  <i className="fas fa-exclamation-triangle"></i>
                  The following GeneIDs were not found:{" "}
                  {invalidGeneIds.join(", ")}
                </div>
              )}
            </div>
          )}

          {/* Results Panel */}
          {activeTab === "results" && (
            <div className={styles.resultsPanel}>
              <div className={styles.resultsPanelHeader}>
                <h2>Gene Ontology Annotations</h2>
                <button
                  className={styles.backButton}
                  onClick={() => setActiveTab("input")}
                >
                  <i className="fas fa-arrow-left"></i> Back to Search
                </button>
              </div>

              {goAnnotations.length > 0 ? (
                <div className={styles.geneResults}>
                  {goAnnotations.map((gene) => (
                    <div key={gene.GeneID} className={styles.geneCard}>
                      <div className={styles.geneHeader}>
                        <h3>
                          <i className="fas fa-dna"></i> {gene.GeneID}
                        </h3>
                      </div>

                      <div className={styles.geneContent}>
                        {/* Display GO annotations by category */}
                        {Object.entries(gene.annotations).map(
                          ([category, terms]) =>
                            terms.length > 0 && (
                              <div
                                key={category}
                                className={`${
                                  styles.categoryBlock
                                } ${getCategoryColor(category)}`}
                              >
                                <div className={styles.categoryHeader}>
                                  <i className={getCategoryIcon(category)}></i>
                                  <h4>
                                    {getCategoryName(category)} ({terms.length})
                                  </h4>
                                </div>

                                <div className={styles.termsList}>
                                  {terms.map((term, idx) => (
                                    <div key={idx} className={styles.termItem}>
                                      <a
                                        href={`http://amigo.geneontology.org/amigo/term/${term.GO_Id}`}
                                        target="_blank"
                                        rel="noopener noreferrer"
                                        className={styles.termId}
                                      >
                                        {term.GO_Id}
                                      </a>
                                      <span className={styles.termName}>
                                        {term.GO_term}
                                      </span>
                                    </div>
                                  ))}
                                </div>
                              </div>
                            )
                        )}

                        {/* If no annotations found for this gene */}
                        {Object.values(gene.annotations).every(
                          (arr) => arr.length === 0
                        ) && (
                          <div className={styles.emptyState}>
                            <i className="fas fa-info-circle"></i>
                            No GO annotations available for this gene.
                          </div>
                        )}
                      </div>
                    </div>
                  ))}
                </div>
              ) : (
                <div className={styles.emptyState}>
                  <i className="fas fa-search"></i>
                  No annotations found. Please try different gene IDs.
                </div>
              )}
            </div>
          )}
        </div>
      </main>
      <Footer />
    </>
  );
};

export default PepperGOPage;
