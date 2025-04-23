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
  const [studyId, setStudyId] = useState("");
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
  const [currentPage, setCurrentPage] = useState(1);
  const [totalPages, setTotalPages] = useState(0);

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
          `/api/snp?studyId=${currentStudyId}&page=${page}&pageSize=500`
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

  // Render pagination controls
  const renderPaginationControls = () => {
    return (
      <div className={styles.paginationControls}>
        <button
          onClick={() => setCurrentPage((page) => Math.max(1, page - 1))}
          disabled={currentPage === 1}
        >
          Previous
        </button>
        <span>
          Page {currentPage} of {totalPages}
        </span>
        <button
          onClick={() =>
            setCurrentPage((page) => Math.min(totalPages, page + 1))
          }
          disabled={currentPage === totalPages}
        >
          Next
        </button>
      </div>
    );
  };

  // Loading state
  if (loading) {
    return (
      <>
        <Header />
        <div className={styles.loadingContainer}>
          <div className={styles.spinner}></div>
          <p>Loading study data...</p>
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
          <p>Error loading study data: {error}</p>
          <button onClick={() => router.push("/snp_marker")}>
            Return to Study Selection
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
      </Head>

      <Header />

      <div className={styles.snpContainer}>
        <h2>SNP Marker Search</h2>
        <p>
          {/* Study ID: {studyId} - Search for SNP markers using Chromosome and */}
          Search for SNP markers using Chromosome and Position Range.
        </p>

        <form onSubmit={handleSearch} className={styles.searchBar}>
          <input
            type="text"
            placeholder="Chromosome (e.g., Pn1)"
            value={chromosome}
            onChange={(e) => setChromosome(e.target.value)}
          />
          {/* {autoStart !== null && autoEnd !== null && (
            <p className={styles.suggestedRange}>
              Suggested range: Start: {autoStart}, End: {autoEnd}
            </p>
          )} */}
          <div className={styles.chromosomeInputs}>
            <input
              type="text"
              placeholder={`Start Position (e.g. ${autoStart || "100"})`}
              value={chromosomeStart}
              onChange={(e) => setChromosomeStart(e.target.value)}
            />
            <input
              type="text"
              placeholder={`End Position (e.g. ${autoEnd || "1000"})`}
              value={chromosomeEnd}
              onChange={(e) => setChromosomeEnd(e.target.value)}
            />
          </div>
          <div className={styles.searchButtons}>
            <button type="submit">Search</button>
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

        {showHeatmap && (
          <SNPHeatmap
            chromosomes={
              filteredResults.length > 0
                ? filteredResults.map((entry) => entry.chromosome)
                : []
            }
            heatmapData={filteredResults.length > 0 ? filteredResults : allData}
          />
        )}

        {noResults ? (
          <div className={styles.noResultsMessage}>
            No results found for your search.
          </div>
        ) : (
          filteredResults.length > 0 && (
            <>
              <div className={styles.visualizationSection}>
                <SNPVisualization
                  snps={filteredResults}
                  start={parseInt(chromosomeStart)}
                  end={parseInt(chromosomeEnd)}
                />
              </div>

              <div className={styles.resultsSection}>
                <h3>Search Results</h3>
                <table className={styles.resultsTable}>
                  <thead>
                    <tr>
                      {/* <th>Variant ID</th> */}
                      <th>Chromosome</th>
                      <th>Position</th>
                      <th>Reference Allele</th>
                      <th>Alternative Allele</th>
                    </tr>
                  </thead>
                  <tbody>
                    {filteredResults.map((result, index) => (
                      <tr key={index}>
                        {/* <td>{result.id}</td> */}
                        <td>{result.chromosome}</td>
                        <td>{result.position}</td>
                        <td>{result.ref}</td>
                        <td>{result.alt.join(", ")}</td>
                      </tr>
                    ))}
                  </tbody>
                </table>
                {/* {renderPaginationControls()} */}
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
