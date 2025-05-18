import argparse
import pandas as pd
import numpy as np
import datetime as dt

import metrics_forecast_class as mfc
import function_logger as fl

from sktime.forecasting.base import ForecastingHorizon

logger=fl.get_logger('get_forecast')
df_forecast_optim=pd.read_csv("../data/forecast_optim.csv")
default_model = 'AutoARIMA'
default_input = "../data/influencer_sample010525_weekly.csv"
# Dummy model definitions (replace with your actual models)
def model_a_predict(data, horizon):
    return ["model_a_pred"] * horizon

def model_b_predict(data, horizon):
    return ["model_b_pred"] * horizon

# Dictionary of available models

def main(csv_path, horizon):
    df = pd.read_csv(csv_path)
    df_dict = dict(tuple(df.groupby('channel_uid')))
    del df
    if 'model_name' not in df_forecast_optim.columns:
        raise ValueError("CSV must contain a 'model_name' column")

    all_predictions = []
    for channel_uid, df_v in df_dict.items():
        logger.info(f"Processing channel {channel_uid}")
        y = df_v['mean_audience'].tail(mfc.max_periods).round()
        y = y.reset_index(drop=True)
        try:
            model_name = df_forecast_optim.loc[df_forecast_optim['channel_uid'] == channel_uid, 'model_name'].values[0]
            model_class = mfc.models_dict.get(model_name,default_model)

            #y.index = pd.RangeIndex(start=0, stop=len(y), step=1)
            #f_h = np.arange(1, horizon + 1)
            fh = ForecastingHorizon(np.arange(1, horizon + 1), is_relative=True)
            model = model_class
            model.fit(y,fh=fh)
            y_pred = model.predict(fh)
        except Exception as e:
            logger.info(f"Error processing channel {channel_uid}: {e}")
            # Fallback to default model
            logger.info(f"Using fallback model for channel {channel_uid}")
            fallback_model_class = mfc.models_dict.get(default_model)
            fallback_model = fallback_model_class
            fallback_model.fit(y)
            y_pred = fallback_model.predict(fh)  
        all_predictions.append({
            'channel_uid': channel_uid,
            'predictions': y_pred.tolist()
        })
    # Convert predictions to DataFrame
    predictions_df = pd.DataFrame(all_predictions)
    # Save predictions to CSV
    predictions_df.to_csv(f"../output/predictions_{dt.datetime.now():%y%m%d%H%M}.csv", index=False)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Predict using models from CSV")
    parser.add_argument("--csv_path", type=str, default=default_input,
                        help="Path to the input CSV file")
    parser.add_argument("--horizon", type=int, default=4,
                        help="Prediction horizon")

    args = parser.parse_args()
    main(args.csv_path, args.horizon)
