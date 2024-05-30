\name{pitcher_pitch_metrics}
\alias{pitcher_pitch_metrics}
\title{Pitcher Pitch Metrics}
\description{
Creates a summary table of pitch metrics for the given file.
Required columns and column names: TaggedPitchType, RelSpeed, SpinAxis, SpinRate, yt_Efficiency, InducedVertBreak, HorzBreak, VertApprAngle, RelHeight, RelSide, Extension. It will alter the original dataframe as well as produce a summarised table.

Columns produced: Pitcher, TaggedPitchType, No., Usage \%, Velo, VeloMax, Tilt, SpinAxis, Tilt_inferred, SpinAxis_inferred, `SpinEff\%`, Vert, Horz, VAA, RelHt, RelSide, Ext
}
\usage{
columns_to_group_by <- c('Pitcher', 'TaggedPitchType')
pitcher_pitch_metrics(data, columns_to_group_by))


}

\arguments{
  \item{data}{
     Must be a dataframe or tibble with column names: TaggedPitchType, RelSpeed, SpinAxis, SpinRate, yt_Efficiency, InducedVertBreak, HorzBreak, VertApprAngle, RelHeight, RelSide, Extension.
}

}
\examples{
# ex 1
pitcher_pitch_metrics(pitcher, c('Pitcher', 'TaggedPitchType'))

# ex 2
columns_to_group_by <- c('Pitcher', 'TaggedPitchType')
pitcher_pitch_metrics(pitcher, columns_to_group_by)

# ex 3
data \%>\%
    pitcher_pitch_metrics(cols_to_group_by = columns_to_group_by)


#If you do not have a column in your data, for example yt_Efficiency (aka, spin efficiency), you can add it as NA prior to running the function, and then remove it from selection

    data \%>\%
        dplyr::mutate(yt_Efficiency = NA) \%>\%
        pitcher_pitch_metrics(columns_to_group_by = columns_to_group_by) \%>\%
        select(-`SpinEff\%`)
}
