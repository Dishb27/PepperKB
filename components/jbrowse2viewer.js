import React, { useEffect, useRef } from "react";
import {
  createViewState,
  JBrowseLinearGenomeView,
} from "@jbrowse/react-linear-genome-view";

const JBrowse2Viewer = () => {
  console.log("Rendering component...");
  const viewerRef = useRef(null);

  const viewState = createViewState({
    assembly: {
      name: "Piper_nigrum.genome",
      sequence: {
        type: "ReferenceSequenceTrack",
        trackId: "refseq_track",
        adapter: {
          type: "BgzipFastaAdapter",
          fastaLocation: {
            uri: "/data/Piper_nigrum.genome.fa.gz",
          },
          faiLocation: {
            uri: "/data/Piper_nigrum.genome.fa.gz.fai",
          },
          gziLocation: {
            uri: "/data/Piper_nigrum.genome.fa.gz.gzi",
          },
        },
      },
    },
    tracks: [
      {
        type: "FeatureTrack",
        trackId: "gff_track",
        name: "Annotations",
        adapter: {
          type: "Gff3TabixAdapter",
          gffGzLocation: {
            uri: "/data/Piper_nigrum_edited.sorted.gff.gz",
          },
          index: {
            location: {
              uri: "/data/Piper_nigrum_edited.sorted.gff.gz.tbi",
            },
            indexType: "TBI",
          },
        },
        assemblyNames: ["Piper_nigrum.genome"],
      },
      {
        type: "VariantTrack",
        trackId: "vcf_track",
        name: "Variants",
        adapter: {
          type: "VcfTabixAdapter",
          vcfGzLocation: {
            uri: "/data/20miss.recode.vcf.gz", 
          },
          index: {
            location: {
              uri: "/data/20miss.recode.vcf.gz.tbi", 
            },
            indexType: "TBI",=
          },
        },
        assemblyNames: ["Piper_nigrum.genome"],
      },
    ],
  });

  useEffect(() => {
    // Ensuring viewerRef is not null before performing any operations
    if (viewerRef.current) {
      console.log("Viewer mounted successfully");
    }
  }, []);

  return (
    <div ref={viewerRef}>
      <JBrowseLinearGenomeView viewState={viewState} />
    </div>
  );
};

export default JBrowse2Viewer;
