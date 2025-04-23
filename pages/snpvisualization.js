import React, { useState } from "react";
import styles from "../styles/snp_visualization.module.css";

const SNPVisualization = ({ snps, start, end }) => {
  const width = 600;
  const height = 200;
  const padding = 50;
  const triangleHeight = 8;
  const triangleBase = 5; // Adjust base width of triangle here
  const tickCount = 5; // Number of ticks (excluding start and end)

  const [tooltip, setTooltip] = useState(null);

  const scaleX = (position) => {
    if (position < start || position > end) {
      console.warn(`SNP position ${position} is out of range.`);
      return null;
    }
    return (
      padding + ((position - start) / (end - start)) * (width - 2 * padding)
    );
  };

  const getTicks = () => {
    const ticks = [];
    const step = (end - start) / (tickCount + 1); // Divide the range evenly
    for (let i = 1; i <= tickCount; i++) {
      ticks.push(start + i * step);
    }
    return ticks;
  };

  const handleTriangleClick = (snp) => {
    if (tooltip?.position === scaleX(snp.position)) {
      // If the tooltip is already displayed for the clicked SNP, hide it
      setTooltip(null);
    } else {
      // Otherwise, show the tooltip
      setTooltip({
        position: scaleX(snp.position),
        y: height / 2 - triangleHeight - 25, // Position tooltip above the triangle
        positionText: `Position: ${snp.position}`,
        alleleText: `${snp.ref} → ${snp.alt}`, // Format Ref → Alt
      });
    }
  };

  const ticks = getTicks();

  return (
    <svg width={width} height={height} className={styles.visualization}>
      {/* Top Heading */}
      <text
        x={width / 2}
        y={20} // Adjust vertical position of the heading
        textAnchor="middle"
        fontSize="16"
        fontWeight="bold"
        fill="black"
      >
        SNP Location
      </text>

      {/* Horizontal line */}
      <line
        x1={padding}
        y1={height / 2}
        x2={width - padding}
        y2={height / 2}
        stroke="black"
        strokeWidth="2"
      />

      {/* Tick marks and labels */}
      {ticks.map((tick, index) => {
        const x = scaleX(tick);
        return (
          <g key={index}>
            {/* Tick mark */}
            <line
              x1={x}
              y1={height / 2 - 5}
              x2={x}
              y2={height / 2 + 5}
              stroke="black"
              strokeWidth="1"
            />
            {/* Tick label */}
            <text
              x={x}
              y={height / 2 + 20}
              textAnchor="middle"
              fontSize="10"
              fill="black"
            >
              {Math.round(tick)}
            </text>
          </g>
        );
      })}

      {/* Start label */}
      <text x={padding} y={height / 2 + 40} textAnchor="middle" fontSize="12">
        {start} bp
      </text>
      {/* End label */}
      <text
        x={width - padding}
        y={height / 2 + 40}
        textAnchor="middle"
        fontSize="12"
      >
        {end} bp
      </text>

      {/* Triangles as SNP markers */}
      {snps.map((snp, index) => {
        const x = scaleX(snp.position);
        if (x === null) return null;

        const points = `${x},${height / 2 - triangleHeight} ${
          x - triangleBase
        },${height / 2 + triangleHeight} ${x + triangleBase},${
          height / 2 + triangleHeight
        }`;

        return (
          <polygon
            key={index}
            points={points}
            fill="#ee2020"
            onClick={() => handleTriangleClick(snp)} // Click event handler
            style={{ cursor: "pointer" }}
          />
        );
      })}

      {/* Tooltip */}
      {tooltip && (
        <g>
          {/* Tooltip Background */}
          <rect
            x={tooltip.position - 70} // Adjust width for better alignment
            y={tooltip.y - 35} // Adjust position for height
            width="140"
            height="40"
            className={styles.tooltip}
          />
          {/* Tooltip Text: Position */}
          <text
            x={tooltip.position}
            y={tooltip.y - 20}
            className={styles["tooltip-text"]}
          >
            {tooltip.positionText}
          </text>
          {/* Tooltip Text: Alleles */}
          <text
            x={tooltip.position}
            y={tooltip.y - 5}
            className={styles["tooltip-text"]}
          >
            {tooltip.alleleText}
          </text>
        </g>
      )}
    </svg>
  );
};

export default SNPVisualization;
