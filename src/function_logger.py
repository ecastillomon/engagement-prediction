import logging
import datetime as dt
import time,sys

def get_logger(name,log_lvl='INFO'):
    log_fmt='%(asctime)s %(message)s'
    logging.Formatter.converter = time.gmtime
    logging.basicConfig(level=log_lvl,format=log_fmt,
                         handlers=[
        logging.FileHandler(f'../log/{name}-log_{dt.date.today()}.txt'),
        logging.StreamHandler()
    ])
    logger=logging.getLogger(name)

    #logger=logger.addHandler(logging.StreamHandler(sys.stdout))
    return logger