import { useRouter } from "next/router";
import React, { useEffect, useState } from "react";
import styles from "../styles/resultblast.module.css";
import Head from "next/head";
import { AlignJustify } from "lucide-react";

const ResultBlast = () => {
  const router = useRouter();
  const [blastResults, setBlastResults] = useState(null);
  const [isLoading, setIsLoading] = useState(true);
  const [error, setError] = useState(null);
  const [program, setProgram] = useState("");
  const [errorMessage, setErrorMessage] = useState("");
  const [programInfo, setProgramInfo] = useState({
    program: "",
    database: "",
  });

  useEffect(() => {
    if (!router.isReady) return;

    setIsLoading(true);

    if (router.query.results) {
      try {
        const parsedResults = JSON.parse(router.query.results);

        // Check if there's an error message
        if (router.query.error) {
          setErrorMessage(router.query.error);
        } else if (parsedResults.message) {
          setErrorMessage(parsedResults.message);
        }

        // Set program info
        if (router.query.program) {
          setProgramInfo({
            program: router.query.program,
            database: router.query.database || "",
          });
          setProgram(router.query.program);
        } else if (parsedResults.program && parsedResults.database) {
          setProgramInfo({
            program: parsedResults.program,
            database: parsedResults.database,
          });
          setProgram(parsedResults.program);
        }

        if (parsedResults?.hits && Array.isArray(parsedResults.hits)) {
          const formattedResults = parsedResults.hits.map((hit) => ({
            description: hit.description || "No description provided",
            score: hit.score || "N/A",
            evalue: hit.evalue || "N/A",
            alignments:
              hit.alignments?.map((alignment) => ({
                querySeq: alignment.querySeq || "N/A",
                hitSeq: alignment.hitSeq || "N/A",
                midline: alignment.midline || "N/A",
                alignLength: alignment.alignLength || "N/A",
                queryFrom: alignment.queryFrom || "N/A",
                queryTo: alignment.queryTo || "N/A",
                hitFrom: alignment.hitFrom || "N/A",
                hitTo: alignment.hitTo || "N/A",
              })) || [],
          }));

          setBlastResults(formattedResults);
        } else {
          setError("Invalid BLAST results format.");
        }
      } catch (error) {
        setError(`Error parsing BLAST results: ${error.message}`);
        setErrorMessage(
          "Failed to load results. Please try your search again."
        );
      } finally {
        setIsLoading(false);
      }
    } else {
      setIsLoading(false);
    }
  }, [
    router.isReady,
    router.query.results,
    router.query.program,
    router.query.database,
    router.query.error,
  ]);

  // Function to get the appropriate unit based on program type
  const getLengthUnit = () => {
    return program === "blastn" ? "bp" : "aa";
  };

  // Component to display errors or no results messages
  const ResultsMessage = () => {
    if (errorMessage) {
      return (
        <div className={styles.resultsMessage}>
          <div className={styles.alertBox}>
            <h3>
              {blastResults && blastResults.length > 0
                ? "Note:"
                : "No Results Found"}
            </h3>
            <p>{errorMessage}</p>

            {/* Display program-database mismatch errors with helpful suggestions */}
            {errorMessage.includes("Incompatible selection") && (
              <div className={styles.incompatibilityHelp}>
                <h4>Program and Database Compatibility:</h4>
                <ul>
                  <li>
                    <strong>blastn</strong> - Use with nucleotide databases
                    (genome or CDS)
                  </li>
                  <li>
                    <strong>blastp</strong> - Use with protein databases only
                  </li>
                </ul>
                <p>Please go back and adjust your selection.</p>
              </div>
            )}

            {/* Display sequence type mismatch errors */}
            {errorMessage.includes("Your sequence appears to be") && (
              <div className={styles.incompatibilityHelp}>
                <h4>Sequence Type Mismatch:</h4>
                <p>
                  The sequence you provided doesn't match the selected program:
                </p>
                <ul>
                  <li>
                    <strong>blastn</strong> - For nucleotide sequences (DNA/RNA)
                  </li>
                  <li>
                    <strong>blastp</strong> - For protein sequences (amino
                    acids)
                  </li>
                </ul>
                <p>Please go back and adjust your selection.</p>
              </div>
            )}

            <button
              onClick={() => router.push("/blast")}
              className={styles.goBackButton}
            >
              Go Back to BLAST Search
            </button>
          </div>
        </div>
      );
    }
    return null;
  };

  // Handle loading state
  if (isLoading) {
    return (
      <div className={styles.container}>
        <div className={styles.loader}>
          <div className={styles.spinnerRing}></div>
          <p>Loading BLAST results...</p>
        </div>
      </div>
    );
  }

  return (
    <>
      <Head>
        <title>BLAST Results - Black Pepper Knowledgebase</title>
        <meta name="description" content="BLAST sequence analysis results" />
      </Head>

      <div className={styles.container}>
        <header className={styles.header}>
          <h1 className={styles.title}>BLAST Search Results</h1>

          {/* Display program information */}
          {programInfo.program && (
            <div className={styles.programInfo}>
              <div className={styles.programTag}>
                <span className={styles.programLabel}>Program:</span>
                <span className={styles.programValue}>
                  {programInfo.program}
                </span>
              </div>
              <div className={styles.programTag}>
                <span className={styles.programLabel}>Database:</span>
                <span className={styles.programValue}>
                  {programInfo.database}
                </span>
              </div>
            </div>
          )}
        </header>

        {/* Display error message or no results message */}
        <ResultsMessage />

        {/* Handle error state */}
        {error && !errorMessage && (
          <div className={styles.errorCard}>
            <div className={styles.errorIcon}>!</div>
            <p>Error: {error}</p>
            <button
              onClick={() => router.push("/blast")}
              className={styles.goBackButton}
            >
              Go Back to BLAST Search
            </button>
          </div>
        )}

        {/* Handle no results */}
        {(!blastResults || blastResults.length === 0) && !errorMessage && (
          <div className={styles.noResults}>
            <div className={styles.noResultsIcon}>
              <svg viewBox="0 0 24 24" width="48" height="48">
                <path
                  fill="currentColor"
                  d="M11,2V4.07C7.38,4.53 4.53,7.38 4.07,11H2V13H4.07C4.53,16.62 7.38,19.47 11,19.93V22H13V19.93C16.62,19.47 19.47,16.62 19.93,13H22V11H19.93C19.47,7.38 16.62,4.53 13,4.07V2M11,6.08V8H13V6.09C15.5,6.5 17.5,8.5 17.92,11H16V13H17.91C17.5,15.5 15.5,17.5 13,17.92V16H11V17.91C8.5,17.5 6.5,15.5 6.08,13H8V11H6.09C6.5,8.5 8.5,6.5 11,6.08M12,11A1,1 0 0,0 11,12A1,1 0 0,0 12,13A1,1 0 0,0 13,12A1,1 0 0,0 12,11Z"
                />
              </svg>
            </div>
            <h3>No Results Found</h3>
            <p>
              No significant matches were found. Try adjusting your search
              parameters.
            </p>
            <button
              onClick={() => router.push("/blast")}
              className={styles.goBackButton}
            >
              Go Back to BLAST Search
            </button>
          </div>
        )}

        {/* Display results if available */}
        {blastResults && blastResults.length > 0 && (
          <div className={styles.resultsGrid}>
            {blastResults.map((hit, hitIndex) => (
              <div key={hitIndex} className={styles.resultCard}>
                <div className={styles.resultHeader}>
                  <h3 className={styles.resultTitle}>{hit.description}</h3>
                  <div className={styles.resultStats}>
                    <div className={styles.statItem}>
                      <span className={styles.statLabel}>Score</span>
                      <span className={styles.statValue}>{hit.score}</span>
                    </div>
                    <div className={styles.statItem}>
                      <span className={styles.statLabel}>E-value</span>
                      <span className={styles.statValue}>{hit.evalue}</span>
                    </div>
                  </div>
                </div>

                <div className={styles.alignmentsContainer}>
                  <h4 className={styles.alignmentsTitle}>
                    <AlignJustify size={18} className={styles.alignmentIcon} />
                    <span>Alignments</span>
                  </h4>

                  {hit.alignments.length > 0 ? (
                    hit.alignments.map((alignment, alignmentIndex) => (
                      <div
                        key={alignmentIndex}
                        className={styles.alignmentCard}
                      >
                        <div className={styles.alignmentInfo}>
                          <div className={styles.rangeInfo}>
                            <div className={styles.rangeItem}>
                              <span className={styles.rangeLabel}>Query</span>
                              <span className={styles.rangeValue}>
                                {alignment.queryFrom} - {alignment.queryTo}
                              </span>
                            </div>
                            <div className={styles.rangeItem}>
                              <span className={styles.rangeLabel}>Hit</span>
                              <span className={styles.rangeValue}>
                                {alignment.hitFrom} - {alignment.hitTo}
                              </span>
                            </div>
                            <div className={styles.rangeItem}>
                              <span className={styles.rangeLabel}>Length</span>
                              <span className={styles.rangeValue}>
                                {alignment.alignLength} {getLengthUnit()}
                              </span>
                            </div>
                          </div>
                        </div>

                        <div className={styles.sequenceViewer}>
                          <div className={styles.sequencePair}>
                            <div className={styles.sequenceLabel}>Query</div>
                            <pre className={styles.querySeq}>
                              {alignment.querySeq}
                            </pre>
                          </div>

                          <div className={styles.sequencePair}>
                            <div className={styles.sequenceLabel}></div>
                            <pre className={styles.midline}>
                              {alignment.midline}
                            </pre>
                          </div>

                          <div className={styles.sequencePair}>
                            <div className={styles.sequenceLabel}>Hit</div>
                            <pre className={styles.hitSeq}>
                              {alignment.hitSeq}
                            </pre>
                          </div>
                        </div>
                      </div>
                    ))
                  ) : (
                    <div className={styles.noAlignments}>
                      <p>No alignments available</p>
                    </div>
                  )}
                </div>
              </div>
            ))}
          </div>
        )}
      </div>
    </>
  );
};

export default ResultBlast;
