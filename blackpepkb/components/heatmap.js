import dynamic from "next/dynamic";

const Plot = dynamic(() => import("react-plotly.js"), { ssr: false });

const Heatmap = ({ chromosomes, heatmapData }) => {
  return (
    <Plot
      data={[
        {
          z: heatmapData,
          x: Array.from(
            { length: Math.max(...heatmapData.map((row) => row.length)) },
            (_, i) => `${i + 1}Mb`
          ),
          y: chromosomes,
          type: "heatmap",
          colorscale: "Viridis",
        },
      ]}
      layout={{
        title: "SNP Distribution Heatmap",
        xaxis: { title: "1Mb Bins" },
        yaxis: { title: "Chromosomes" },
        height: 600,
        width: 1000,
      }}
    />
  );
};

export default Heatmap;
