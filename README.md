### Social Media Engagement Forecasting and Audience Growth — Master's Thesis

**Program**: Econometrics, Operations Research and Actuarial Studies, University of Groningen

This repository contains the code and materials for a thesis studying how well future engagement and audience growth can be modeled from historical performance across TikTok, Instagram, Facebook, and YouTube.

### Abstract

As social media platforms increasingly rely on engagement-driven algorithms to determine content visibility, understanding and predicting user interactions has become a critical challenge for creators and strategists. This thesis investigates how well future engagement and audience growth can be modeled based on historical performance, using a large dataset spanning TikTok, Instagram, Facebook, and YouTube. The study focuses on two core objectives: forecasting potential audience reach through time series models, and explaining short-term audience growth using multivariate regression.

For the forecasting component, a range of models was evaluated—including classical time series methods (AR, ARIMA, ETS, Theta), linear regressions with lagged trends, and deep learning approaches (LSTM, RNN). Engagement metrics such as likes, shares, comments, and views were modeled separately using a rolling forecast origin procedure, with performance assessed through MAE, RMSE, and MAPE. Results varied by platform and metric, with simple models like moving averages and linear regressions outperforming more complex neural architectures in most cases. However, deep learning models showed advantages on longer time horizons, particularly in capturing non-linear patterns.

In the explanatory analysis, linear regressions were used to evaluate the relationship between weekly audience growth and two key predictors: normalized engagement rate and posting frequency. The models revealed strong platform-specific effects. Normalized engagement rate was a significant predictor on Facebook, suggesting that per-capita interaction plays a role in algorithmic amplification. On TikTok and YouTube, posting frequency had a larger impact, suggesting that how often content is posted plays a more influential role than engagement on these platforms. No significant effects were observed on Instagram for either variable.

Together, the findings offer both methodological insights and practical implications. They highlight the importance of aligning modeling techniques with platform dynamics, and suggest that effective growth strategies must be tailored to the distinct logic of each social media environment.

### Repository overview

- `src/`: Python code to run rolling-origin time series validation and generate out-of-sample forecasts.
  - `metrics_forecast_class.py`: Implements model catalog and cross-validation over multiple channels and metrics using `sktime`.
  - `get_forecast.py`: Loads the best model per channel and produces multi-step forecasts for production-style inference.
  - `function_logger.py`: Simple file and console logger; writes logs to `log/`.
  - Notebooks (`*.ipynb`): EDA and experiment runs.
- `lib/` and `R/`: R code used for additional analyses (e.g., linear models, explanatory regressions) used in the thesis.
- `output/`: Generated summaries, model comparisons, and forecast files.
- `figure/`, `Thesis/`: Figures and the LaTeX thesis sources.

### Data expectations

Forecasting expects a panel of weekly, per-channel observations. The Python code in `src/` expects at minimum:

- `channel_uid`: unique identifier per channel
- `published_date`: timestamp (weekly granularity recommended)
- `source`: platform name (e.g., TikTok, Instagram, Facebook, YouTube)
- One or more numeric metric columns to forecast (e.g., `mean_audience`)

For batch forecasting with `get_forecast.py`, the following CSVs are used:

- Input series CSV: must contain the columns above. For the provided script, the target series for forecasting is `mean_audience`.
- Model selection file (default `data/forecast_optim.csv`): mapping of best model per channel, with at least:
  - `channel_uid`
  - `model_name` (one of the names listed under Available models below)

### Available models (as implemented in `src/metrics_forecast_class.py`)

- AutoARIMA
- ThetaForecaster (Theta)
- AutoETS (ETS)
- Prophet (multiplicative seasonality)
- AutoREG with lags 2–5
- ARIMA variants: AR(p) with p ∈ {2,3,4,5} and MA(q) with q ∈ {2,3,4,5}
- NeuralForecastLSTM and NeuralForecastRNN (optional; require extra dependencies)

Scoring metrics used in cross-validation: Mean Absolute Error (MAE), Root Mean Squared Error (RMSE), and Mean Absolute Percentage Error (MAPE).

Rolling-origin setup (defaults in code):

- Expanding window CV with `initial_window=12`, `step_length=6`, forecast horizon `h=6`.

### Environment setup

Python 3.10+ recommended.

```bash
python -m venv .venv
source .venv/bin/activate
pip install -U pip

# Core
pip install pandas numpy tqdm sktime

# Optional models (install as needed)
pip install prophet
pip install neuralforecast torch --index-url https://download.pytorch.org/whl/cpu
```

Notes:
- If `prophet` or `neuralforecast` fail to install on your platform, you can omit them and exclude those models from the run.
- `sktime` manages model evaluation and CV; some estimators pull in extra dependencies.

### Running rolling-origin evaluation

The evaluation API is provided via `MultiSeriesEvaluator` and `TimeSeriesEvaluator` in `src/metrics_forecast_class.py`.

```python
import pandas as pd
from src.metrics_forecast_class import MultiSeriesEvaluator, models_dict

# Load your weekly panel
df = pd.read_csv("data/your_timeseries.csv")

# Choose which numeric columns to forecast per channel
metric_cols = ["mean_audience"]  # add more metrics if available

evaluator = MultiSeriesEvaluator(df, date_col="published_date", channel_col="channel_uid")
evaluator.run(
    metric_cols=metric_cols,
    h=6,
    initial_window=12,
    step_length=6,
    models=["AutoARIMA", "Theta", "ETS"],  # pick a subset supported by your env
    models_dict=models_dict,
    max_periods=None,  # or set an integer cap on history length
)

# Aggregate results across channels/metrics/models
summary = evaluator.get_summary()
summary.to_csv("output/summary_df.csv", index=False)

# Full per-fold results if needed
summary_complete = evaluator.get_summary_complete()
summary_complete.to_csv("output/summary_complete_df.csv", index=False)
```

The logger writes progress to `log/metrics_forecast-log_<date>.txt`.

### Producing forecasts with selected models

Given a per-channel model selection file (e.g., from prior evaluation), you can produce multi-step forecasts:

```bash
python src/get_forecast.py --csv_path data/your_timeseries.csv --horizon 4
```

Behavior:
- Loads `data/forecast_optim.csv` to map `channel_uid → model_name`.
- Fits the specified model per channel on the latest history of `mean_audience` and predicts `--horizon` steps ahead.
- Saves a timestamped CSV to `output/` with columns: `channel_uid`, `predictions` (list of horizon values).
- Falls back to `AutoARIMA` if a model fails for a particular channel.

Expected schema for `data/forecast_optim.csv`:

```csv
channel_uid,model_name
abc123,AutoARIMA
def456,ETS
```

### Notebooks

- `src/eda_forecast.ipynb`, `src/eda_forecast_v2.ipynb`: exploratory analysis and model probes
- `src/main_forecast_local.ipynb`, `src/main_forecast_collab.ipynb`: end-to-end experiment runs

### Outputs

- `output/summary_df_*.csv`: aggregated CV metrics by model, channel, and metric
- `output/predictions_*.csv`: out-of-sample forecasts per channel
- `log/*`: run logs

### Reproducing thesis results

- Use the evaluation workflow above to compute model performance per platform and metric.
- Use the R code in `lib/` or `R/` for explanatory regressions on audience growth, as referenced in the thesis text.
- Figures used in the document are in `figure/` and `Thesis/images/`.

### Project structure

```text
thesis/
  src/                   # Python forecasting code and notebooks
  lib/, R/               # R scripts for additional analyses
  output/, figure/, log/ # generated artifacts and logs
  Thesis/                # LaTeX sources of the thesis
  data/                  # input datasets (not tracked)
```

### License

If you plan to release, add a LICENSE file and reference it here. If this is private academic work, restrict distribution accordingly.
