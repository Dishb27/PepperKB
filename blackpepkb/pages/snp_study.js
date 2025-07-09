import React, { useState, useEffect } from "react";
import Head from "next/head";
import { useRouter } from "next/router";
import styles from "../styles/snp_markers.module.css";
import SNPHeatmap from "./snp_heatmap";
import SNPVisualization from "./snpvisualization";
import Header from "../components/header";
import Footer from "../components/footer";

const SNPMarkers = () => {
  const router = useRouter();
  const { study } = router.query;

  // State variables
  const [, setStudyId] = useState("");
  const [studyName, setStudyName] = useState("");
  const [chromosome, setChromosome] = useState("");
  const [chromosomeStart, setChromosomeStart] = useState("");
  const [chromosomeEnd, setChromosomeEnd] = useState("");
  const [autoStart, setAutoStart] = useState(null);
  const [autoEnd, setAutoEnd] = useState(null);
  const [filteredResults, setFilteredResults] = useState([]);
  const [noResults, setNoResults] = useState(false);
  const [allData, setAllData] = useState([]);
  const [showHeatmap, setShowHeatmap] = useState(true);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);
  const [, setCurrentPage] = useState(1);
  const [, setTotalPages] = useState(0);
  const [isMobile, setIsMobile] = useState(false);

  // Check if device is mobile
  useEffect(() => {
    const checkMobile = () => {
      setIsMobile(window.innerWidth < 768);
    };

    checkMobile();
    window.addEventListener("resize", checkMobile);

    return () => window.removeEventListener("resize", checkMobile);
  }, []);

  // Fetch study data with pagination
  const fetchStudyData = async (page = 1) => {
    if (!study) return;

    setLoading(true);
    try {
      // Fetch study details
      const studyResponse = await fetch(`/api/studies?studyName=${study}`);
      if (!studyResponse.ok) {
        throw new Error(`HTTP error! Status: ${studyResponse.status}`);
      }
      const studyData = await studyResponse.json();

      if (studyData.length > 0) {
        const currentStudyId = studyData[0].study_id;
        setStudyId(currentStudyId);
        setStudyName(studyData[0].study_name);

        // Fetch SNP data with pagination
        const snpResponse = await fetch(
          `/api/snp?studyId=${currentStudyId}&page=${page}&pageSize=20000`
        );
        if (!snpResponse.ok) {
          throw new Error(`HTTP error! Status: ${snpResponse.status}`);
        }
        const snpData = await snpResponse.json();

        // Transform data
        const formattedData = snpData.data.map((entry) => ({
          chromosome: entry.CHROM,
          position: parseInt(entry.POS),
          id: entry.variant_id,
          ref: entry.REF,
          alt: [entry.ALT],
          qual: 0,
        }));

        setAllData((prevData) =>
          page === 1 ? formattedData : [...prevData, ...formattedData]
        );
        setTotalPages(snpData.pagination.totalPages);
        setCurrentPage(snpData.pagination.currentPage);

        // If there are more pages, fetch next page
        if (page < snpData.pagination.totalPages) {
          await fetchStudyData(page + 1);
        } else {
          setLoading(false);
        }
      } else {
        throw new Error("Study not found");
      }
    } catch (error) {
      console.error("Error fetching study data:", error);
      setError(error.message);
      setLoading(false);
    }
  };

  // Update auto range when chromosome or data changes
  useEffect(() => {
    if (chromosome && allData.length > 0) {
      const chromosomeData = allData.filter(
        (entry) => entry.chromosome.toLowerCase() === chromosome.toLowerCase()
      );
      if (chromosomeData.length > 0) {
        const positions = chromosomeData.map((entry) => entry.position);
        setAutoStart(Math.min(...positions));
        setAutoEnd(Math.max(...positions));
      } else {
        setAutoStart(null);
        setAutoEnd(null);
      }
    } else {
      setAutoStart(null);
      setAutoEnd(null);
    }
  }, [chromosome, allData]);

  // Fetch study data when the component mounts or study changes
  useEffect(() => {
    fetchStudyData();
  }, [study]);

  // Handle search functionality
  const handleSearch = (e) => {
    e.preventDefault();
    const start = parseInt(chromosomeStart);
    const end = parseInt(chromosomeEnd);

    if (!chromosome || isNaN(start) || isNaN(end)) {
      alert(
        "Please provide chromosome, valid start position, and valid end position."
      );
      return;
    }

    if (start >= end) {
      alert("Start position must be less than end position.");
      return;
    }

    const results = allData.filter(
      (entry) =>
        entry.chromosome.toLowerCase() === chromosome.toLowerCase() &&
        entry.position >= start &&
        entry.position <= end
    );

    setFilteredResults(results);
    setNoResults(results.length === 0);
    setShowHeatmap(false);
  };

  // Handle reset functionality
  const handleReset = () => {
    setChromosome("");
    setChromosomeStart("");
    setChromosomeEnd("");
    setAutoStart(null);
    setAutoEnd(null);
    setFilteredResults([]);
    setNoResults(false);
    setShowHeatmap(true);
  };

  // Handle auto-fill suggested range
  const handleAutoFill = () => {
    if (autoStart !== null && autoEnd !== null) {
      setChromosomeStart(autoStart.toString());
      setChromosomeEnd(autoEnd.toString());
    }
  };

  // Get unique chromosomes for dropdown
  const getUniqueChromosomes = () => {
    const chromosomes = [...new Set(allData.map((entry) => entry.chromosome))];
    return chromosomes.sort();
  };

  // Export results to CSV
  const exportToCSV = () => {
    if (filteredResults.length === 0) return;

    const headers = [
      "Chromosome",
      "Position",
      "Reference Allele",
      "Alternative Allele",
    ];
    const csvContent = [
      headers.join(","),
      ...filteredResults.map((result) =>
        [
          result.chromosome,
          result.position,
          result.ref,
          result.alt.join(";"),
        ].join(",")
      ),
    ].join("\n");

    const blob = new Blob([csvContent], { type: "text/csv" });
    const url = window.URL.createObjectURL(blob);
    const a = document.createElement("a");
    a.href = url;
    a.download = `${studyName}_SNP_results_${chromosome}_${chromosomeStart}-${chromosomeEnd}.csv`;
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    window.URL.revokeObjectURL(url);
  };

  // Render pagination controls (if needed for future use)
  // const renderPaginationControls = () => {
  //   return (
  //     <div className={styles.paginationControls}>
  //       <button
  //         onClick={() => setCurrentPage((page) => Math.max(1, page - 1))}
  //         disabled={currentPage === 1}
  //       >
  //         Previous
  //       </button>
  //       <span>
  //         Page {currentPage} of {totalPages}
  //       </span>
  //       <button
  //         onClick={() =>
  //           setCurrentPage((page) => Math.min(totalPages, page + 1))
  //         }
  //         disabled={currentPage === totalPages}
  //       >
  //         Next
  //       </button>
  //     </div>
  //   );
  // };

  // Loading state
  if (loading) {
    return (
      <>
        <Header />
        <div className={styles.loadingContainer}>
          <div className={styles.spinner}></div>
          <p>Loading study data...</p>
          <p style={{ fontSize: "14px", color: "#666", marginTop: "10px" }}>
            Please wait while we fetch SNP data...
          </p>
        </div>
        <Footer />
      </>
    );
  }

  // Error state
  if (error) {
    return (
      <>
        <Header />
        <div className={styles.errorContainer}>
          <h2>Error Loading Data</h2>
          <p>Error loading study data: {error}</p>
          <button onClick={() => router.push("/snp_markers")}>
            Return to Study Selection
          </button>
          <button
            onClick={() => window.location.reload()}
            style={{ marginLeft: "10px" }}
          >
            Retry
          </button>
        </div>
        <Footer />
      </>
    );
  }

  return (
    <>
      <Head>
        <title>{studyName} SNP Markers - PepperKB</title>
        <meta
          name="description"
          content={`Search SNP markers for ${studyName} study`}
        />
        <meta name="viewport" content="width=device-width, initial-scale=1" />
      </Head>

      <Header />

      <div className={styles.snpContainer}>
        <h2>SNP Marker Search</h2>
        <p>
          Search for SNP markers by specifying chromosome and position range
        </p>
        <p style={{ fontSize: "14px", color: "#666", textAlign: "center" }}>
          Total SNPs loaded: {allData.length.toLocaleString()}
        </p>

        <form onSubmit={handleSearch} className={styles.searchBar}>
          {/* Chromosome input with datalist for suggestions */}
          <div
            style={{ position: "relative", width: "100%", maxWidth: "300px" }}
          >
            <input
              type="text"
              placeholder="Chromosome (e.g., Pn1)"
              value={chromosome}
              onChange={(e) => setChromosome(e.target.value)}
              list="chromosomes"
            />
            <datalist id="chromosomes">
              {getUniqueChromosomes().map((chr) => (
                <option key={chr} value={chr} />
              ))}
            </datalist>
          </div>

          {/* Suggested range display */}
          {autoStart !== null && autoEnd !== null && (
            <div className={styles.suggestedRange}>
              Suggested range for {chromosome}: {autoStart.toLocaleString()} -{" "}
              {autoEnd.toLocaleString()}
              <button
                type="button"
                onClick={handleAutoFill}
                style={{
                  marginLeft: "10px",
                  padding: "4px 8px",
                  fontSize: "12px",
                  backgroundColor: "#196f5f",
                  color: "white",
                  border: "none",
                  borderRadius: "4px",
                  cursor: "pointer",
                }}
              >
                Use Range
              </button>
            </div>
          )}

          <div className={styles.chromosomeInputs}>
            <input
              type="number"
              placeholder={`Start Position (e.g. ${autoStart || "100"})`}
              value={chromosomeStart}
              onChange={(e) => setChromosomeStart(e.target.value)}
              min="1"
            />
            <input
              type="number"
              placeholder={`End Position (e.g. ${autoEnd || "1000"})`}
              value={chromosomeEnd}
              onChange={(e) => setChromosomeEnd(e.target.value)}
              min="1"
            />
          </div>

          <div className={styles.searchButtons}>
            <button type="submit">Search SNPs</button>
            <button type="button" onClick={handleReset}>
              Reset
            </button>
            <button
              type="button"
              onClick={() => router.push("/snp_markers")}
              className={styles.backButton}
            >
              Back to Studies
            </button>
          </div>
        </form>

        {/* Heatmap section */}
        {showHeatmap && allData.length > 0 && (
          <div className={styles.visualizationSection}>
            {/* <h3>SNP Distribution Heatmap</h3> */}
            <SNPHeatmap
              chromosomes={
                filteredResults.length > 0
                  ? filteredResults.map((entry) => entry.chromosome)
                  : []
              }
              heatmapData={
                filteredResults.length > 0 ? filteredResults : allData
              }
            />
          </div>
        )}

        {/* Results section */}
        {noResults ? (
          <div className={styles.noResultsMessage}>
            <h3>No Results Found</h3>
            <p>No SNP markers found for your search criteria.</p>
            <p>Try adjusting your chromosome or position range.</p>
          </div>
        ) : (
          filteredResults.length > 0 && (
            <>
              {/* SNP Visualization */}
              <div className={styles.visualizationSection}>
                {/* <h3>SNP Visualization</h3> */}
                <SNPVisualization
                  snps={filteredResults}
                  start={parseInt(chromosomeStart)}
                  end={parseInt(chromosomeEnd)}
                />
              </div>

              {/* Results Table */}
              <div className={styles.resultsSection}>
                <div
                  style={{
                    display: "flex",
                    justifyContent: "space-between",
                    alignItems: "center",
                    flexWrap: "wrap",
                    gap: "10px",
                    marginBottom: "20px",
                  }}
                >
                  <h3>
                    Search Results ({filteredResults.length.toLocaleString()}{" "}
                    SNPs found)
                  </h3>
                  <button
                    onClick={exportToCSV}
                    style={{
                      padding: "8px 16px",
                      backgroundColor: "#27ae60",
                      color: "white",
                      border: "none",
                      borderRadius: "4px",
                      cursor: "pointer",
                      fontSize: "14px",
                    }}
                  >
                    Export CSV
                  </button>
                </div>

                {/* Table container with scrolling */}
                <div className={styles.tableContainer}>
                  <table className={styles.resultsTable}>
                    <thead>
                      <tr>
                        <th>Chromosome</th>
                        <th>Position</th>
                        <th>Reference Allele</th>
                        <th>Alternative Allele</th>
                      </tr>
                    </thead>
                    <tbody>
                      {filteredResults.map((result, index) => (
                        <tr
                          key={`${result.chromosome}-${result.position}-${index}`}
                        >
                          <td>{result.chromosome}</td>
                          <td>{result.position.toLocaleString()}</td>
                          <td>{result.ref}</td>
                          <td>{result.alt.join(", ")}</td>
                        </tr>
                      ))}
                    </tbody>
                  </table>
                </div>

                {/* Mobile scroll hint */}
                {isMobile && (
                  <div
                    style={{
                      fontSize: "12px",
                      color: "#666",
                      textAlign: "center",
                      marginTop: "10px",
                      fontStyle: "italic",
                    }}
                  >
                    💡 Scroll horizontally to view all columns
                  </div>
                )}

                {/* Results summary */}
                <div
                  style={{
                    marginTop: "15px",
                    padding: "10px",
                    backgroundColor: "#f8f9fa",
                    borderRadius: "5px",
                    fontSize: "14px",
                    color: "#666",
                  }}
                >
                  <strong>Search Summary:</strong> Found{" "}
                  {filteredResults.length.toLocaleString()} SNPs on chromosome{" "}
                  {chromosome} between positions{" "}
                  {parseInt(chromosomeStart).toLocaleString()}
                  {" and "}
                  {parseInt(chromosomeEnd).toLocaleString()}
                </div>
              </div>
            </>
          )
        )}
      </div>
      <Footer />
    </>
  );
};

export default SNPMarkers;
