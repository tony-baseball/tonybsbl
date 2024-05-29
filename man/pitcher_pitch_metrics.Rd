\name{pitcher_pitch_metrics}
\alias{pitcher_pitch_metrics}
\title{Pitcher Pitch Metrics}
\description{
Creates a summary table of pitch metrics for the given file.
Required columns and column names: TaggedPitchType, RelSpeed, SpinAxis, SpinRate, yt_Efficiency, InducedVertBreak, HorzBreak, VertApprAngle, RelHeight, RelSide, Extension.
}
\usage{
pitcher_pitch_metrics(data \%>\% dplyr::group_by(TaggedPitchType, Pitcher))

data \%>\%
    dplyr::group_by(TaggedPitchType, Pitcher) \%>\%
    pitcher_pitch_metrics()
}

\arguments{
  \item{data}{
     Must be a dataframe or tibble. Data must be grouped by TaggedPitchType (and optionally Pitcher if more than one pitcher in data)
     prior to running the function. See examples
}

}
\examples{
data \%>\%
    # this is required prior to running the function
    dplyr::group_by(TaggedPitchType, Pitcher) \%>\%
    pitcher_pitch_metrics()

    Will output the following:
      Pitcher    Pitch   No. `Usage \%`  Velo   VeloMax   Tilt   Spin `SpinEff\%`   Vert    Horz     VAA    RelHt  RelSide   Ext
      Name       FB      100      71.9    89.8    91.8    1:04   2273         87    18.2     9.4    -6.9     5.4     2.3     6.6
      Name       SL       33      23.7    79.9    83      8:08   2479         39    -1.9    -7     -11.9     5.4     2.2     6.2
      Name       CH        6       4.3    84      85.2    1:28   1714         68    12      7.9    -10.7     5.2     2.7     6.6

If you do not have a column in your data, for example yt_Efficiency (aka, spin efficiency), you can add it as NA prior to running the function, and then remove it from selection

    data \%>\%
        dplyr::mutate(yt_Efficiency = NA) \%>\%
        dplyr::group_by(TaggedPitchType, Pitcher) \%>\%
        pitcher_pitch_metrics() \%>\%
        select(-`SpinEff\%`)

   Will output the following:
      Pitcher    Pitch   No. `Usage \%`  Velo   VeloMax   Tilt   Spin     Vert    Horz     VAA    RelHt  RelSide   Ext
      Name       FB      100      71.9    89.8    91.8    1:04   2273     18.2     9.4    -6.9     5.4     2.3     6.6
      Name       SL       33      23.7    79.9    83      8:08   2479     -1.9    -7     -11.9     5.4     2.2     6.2
      Name       CH        6       4.3    84      85.2    1:28   1714     12      7.9    -10.7     5.2     2.7     6.6
}
