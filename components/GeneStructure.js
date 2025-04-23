import React, { useEffect, useRef } from "react";
import PropTypes from "prop-types";
import * as d3 from "d3";
import styles from "../styles/genes_structure.module.css";

const GeneStructure = ({
  geneRange,
  exons,
  introns,
  exonStyles,
  intronStyles,
  legendStyles,
}) => {
  const svgRef = useRef(null);

  useEffect(() => {
    if (!geneRange || !exons) {
      console.error("Invalid gene data provided");
      return;
    }

    // Parse gene range
    const [geneStart, geneEnd] = geneRange.split(":").map(Number);
    if (isNaN(geneStart) || isNaN(geneEnd)) {
      console.error("Invalid gene range format.");
      return;
    }

    // Select the SVG container and clear it
    const svg = d3.select(svgRef.current);
    svg.selectAll("*").remove(); // Clear previous render

    const width = 600;
    const height = 100;
    const margin = { top: 20, right: 20, bottom: 20, left: 20 };

    // Scales to map gene range to SVG width
    const scale = d3
      .scaleLinear()
      .domain([geneStart, geneEnd])
      .range([margin.left, width - margin.right]);

    // Draw gene baseline
    svg
      .append("line")
      .attr("class", "gene-baseline")
      .attr("x1", scale(geneStart))
      .attr("x2", scale(geneEnd))
      .attr("y1", height / 2)
      .attr("y2", height / 2)
      .style("stroke", "#E0E0E0")
      .style("stroke-width", 2);

    // Draw introns if they exist
    if (introns && introns.length > 0) {
      svg
        .selectAll(".intron")
        .data(introns)
        .enter()
        .append("line")
        .attr("class", "intron")
        .attr("x1", (d) => {
          // Check if d is a string that can be split
          if (typeof d === "string" && d.includes(":")) {
            const [start, end] = d.split(":");
            return scale(parseInt(start));
          }
          // If d is an object with start property
          else if (d && typeof d === "object" && d.start) {
            return scale(parseInt(d.start));
          }
          // Fallback
          else {
            console.error("Invalid intron format:", d);
            return 0;
          }
        })
        .attr("x2", (d) => {
          // Check if d is a string that can be split
          if (typeof d === "string" && d.includes(":")) {
            const [start, end] = d.split(":");
            return scale(parseInt(end));
          }
          // If d is an object with end property
          else if (d && typeof d === "object" && d.end) {
            return scale(parseInt(d.end));
          }
          // Fallback
          else {
            console.error("Invalid intron format:", d);
            return 0;
          }
        })
        .attr("y1", height / 2)
        .attr("y2", height / 2)
        .style("stroke", intronStyles?.strokeColor || "gray")
        .style("stroke-width", intronStyles?.strokeWidth || 4);
    }

    // Draw exons
    svg
      .selectAll(".exon")
      .data(exons)
      .enter()
      .append("rect")
      .attr("class", "exon")
      .attr("x", (d) => {
        // Check if d is a string that can be split
        if (typeof d === "string" && d.includes(":")) {
          const [start, end] = d.split(":");
          return scale(parseInt(start));
        }
        // If d is an object with start property
        else if (d && typeof d === "object" && d.start) {
          return scale(parseInt(d.start));
        }
        // Fallback
        else {
          console.error("Invalid exon format:", d);
          return 0;
        }
      })
      .attr("y", height / 2 - (exonStyles?.height || 20) / 2) // Center vertically
      .attr("width", (d) => {
        // Check if d is a string that can be split
        if (typeof d === "string" && d.includes(":")) {
          const [start, end] = d.split(":");
          return scale(parseInt(end)) - scale(parseInt(start));
        }
        // If d is an object with start and end properties
        else if (d && typeof d === "object" && d.start && d.end) {
          return scale(parseInt(d.end)) - scale(parseInt(d.start));
        }
        // Fallback
        else {
          console.error("Invalid exon format:", d);
          return 0;
        }
      })
      .attr("height", exonStyles?.height || 20) // Exon height
      .style("fill", exonStyles?.fillColor || "#37a496"); // Exon color
  }, [geneRange, exons, introns, exonStyles, intronStyles]); // Redraw whenever these props change

  return (
    <div className={styles.geneStructureContainer}>
      <svg ref={svgRef} width="800" height="150"></svg>

      {/* Always render the legend with default styles if custom ones aren't provided */}
      <div
        className={legendStyles?.legendClass || styles.defaultLegendContainer}
      >
        <div className={styles.defaultLegend}>
          <div
            className={
              legendStyles?.legendRectClass || styles.defaultLegendRect
            }
            style={{ backgroundColor: exonStyles?.fillColor || "#37a496" }}
          />
          <span
            className={
              legendStyles?.legendTextClass || styles.defaultLegendText
            }
          >
            Exon
          </span>
        </div>

        <div className={styles.defaultLegend}>
          <div
            className={
              legendStyles?.legendRectClass || styles.defaultLegendRect
            }
            style={{
              backgroundColor: intronStyles?.strokeColor || "gray",
            }}
          />
          <span
            className={
              legendStyles?.legendTextClass || styles.defaultLegendText
            }
          >
            Intron
          </span>
        </div>
      </div>
    </div>
  );
};

export default GeneStructure;
