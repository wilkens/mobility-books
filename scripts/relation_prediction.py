"""
Wrapper script to train a model for spatial relation prediction. 
"""
import argparse
import os
import numpy as np
import pandas as pd
import torch
torch.manual_seed (96)

from sklearn.model_selection import train_test_split
import sys, os

if os.path.abspath ("../") not in sys.path:
    sys.path.append (os.path.abspath ("../"))

from modules.relation_prediction import BERTRelationPrediction
from modules.relation_prediction_constants import * 
import logging

logging.basicConfig (
    level=logging.INFO,
    format='%(asctime)s %(message)s'
)

def preprocess_valid_relation_prediction (annotations, *args, **kwargs):
    labels = kwargs.get ("labels", ALL_LABELS)
    full_df = annotations[kwargs.get ("window_size", 10)]
    full_df = full_df.query ("`Spatial Relation` != ''")
    full_df = full_df.query ("`Spatial Relation` in @labels")
    full_df.loc[:,kwargs.get("label_field", "Valid Relation")] = full_df.apply (lambda x: VALID_LABELS[int(x["Spatial Relation"] in SPATIAL_LABELS)], axis=1)

    test_ids_file = kwargs.get ("test_ids_file", "")

    if test_ids_file == "":
        train_df, test_df = train_test_split(full_df, 
                                             test_size=1-kwargs.get ("training_frac", .8), 
                                             random_state=96)
    else:
        with open (test_ids_file) as fin:
            test_ids = [line.strip() for line in fin]
        train_df = full_df.query ("ID not in @test_ids")
        test_df = full_df.query ("ID in @test_ids")

    logging.info (f"All records: {len (full_df)}; for training: {len (train_df)}, for testing: {len (test_df)}")
    return full_df, train_df, test_df

def preprocess_spatial_relation_collapsed_prediction (annotations, *args, **kwargs):
    labels = kwargs.get ("labels", ALL_LABELS)
    full_df = annotations[kwargs.get ("window_size", 10)]
    full_df = full_df.query ("`Spatial Relation` != ''")
    full_df = full_df.query ("`Spatial Relation` in @labels")
    full_df.loc[:,kwargs.get("label_field", "Spatial Relation")] = full_df.apply (lambda x: SPATIAL_RELATION_COLLAPSED_MAP[x["Spatial Relation"]], axis=1)

    test_ids_file = kwargs.get ("test_ids_file", "")
    if test_ids_file == "":
        train_df, test_df = train_test_split(full_df, 
                                             test_size=1-kwargs.get ("training_frac", .8), 
                                             random_state=96)
    else:
        with open (test_ids_file) as fin:
            test_ids = [line.strip() for line in fin]
        train_df = full_df.query ("ID not in @test_ids")
        test_df = full_df.query ("ID in @test_ids")

    logging.info (f"All records: {len (full_df)}; for training: {len (train_df)}, for testing: {len (test_df)}")
    return full_df, train_df, test_df

def preprocess_valid_relation_prediction (train_data_file, 
                                            train_labels_file,
                                            dev_data_file,
                                            dev_labels_file,
                                            *args, **kwargs):
    train_data = pd.read_csv (train_data_file, sep="\t", on_bad_lines="skip")
    train_labels = pd.read_csv (train_labels_file, sep="\t", on_bad_lines="skip")

    # Merge the two dataframes
    train_df = pd.merge (train_data, train_labels, how="inner", on="ID")
    train_df = train_df.head (kwargs.get ("num_training_examples", 100))

    dev_data = pd.read_csv (dev_data_file, sep="\t", on_bad_lines="skip")
    dev_labels = pd.read_csv (dev_labels_file, sep="\t", on_bad_lines="skip")

    # Merge the two dataframes
    dev_df = pd.merge (dev_data, dev_labels, how="inner", on="ID")

    return pd.concat ((train_df, dev_df), axis=1), train_df, dev_df

def readArgs ():
    parser = argparse.ArgumentParser (description="Script to train and evaluate a spatial relation prediction model")
    parser.add_argument ("--task-name", 
                         required=False, choices={"validity", 
                                                 "spatial", 
                                                 "spatial-collapsed", 
                                                 "temporal_span", 
                                                 "narrative_tense"}, 
                         default="validity", 
                         type=str, 
                         help="Name of the task")
    
    parser.add_argument ("--pretrained-model-name", 
                         required=False, 
                         default= "bert-base-cased", 
                         type=str, 
                         help="Name of the pretrained model")
    
    parser.add_argument ("--dims", 
                         required=False, 
                         type=int, 
                         default=768, 
                         help="Size of contextual embedding")
    
    parser.add_argument ("--train-data-file",
                         required=True,
                         type=str,
                         help="Training data in this file")
    
    parser.add_argument ("--train-labels-file",
                         required=True,
                         type=str,
                         help="The training labels in this file")
    
    parser.add_argument ("--dev-data-file",
                         required=True,
                         type=str,
                         help="Development data in this file")
    
    parser.add_argument ("--dev-labels-file",
                         required=True,
                         type=str,
                         help="The development labels in this file")
    
    parser.add_argument ("--num-training-examples", 
                         required=False, 
                         default=100, 
                         type=int,
                         help="Number of training examples")
    
    parser.add_argument ("--learning-rate", 
                         required=False, 
                         default=1e-5, 
                         type=float,
                         help="Learning rate")
    
    parser.add_argument ("--num-epochs", 
                         required=False, 
                         default=10, 
                         type=int, 
                         help="Number of epochs for training")
    
    parser.add_argument ("--num-hidden", 
                         required=False, 
                         default=0, 
                         type=int, 
                         help="Number of hidden layers in the network")
    
    parser.add_argument ("--text-field", 
                         required=False, 
                         default="context_100", 
                         type=str, 
                         help="Column name that contains the entire text")
    
    parser.add_argument ("--models-dir", 
                         required=True, 
                         type=str, 
                         help="Directory in which to store the models")
    
    parser.add_argument ("--results-dir",
                         required=False,
                         type=str,
                         default="",
                         help="Directory in which to store the results")
    
    args = parser.parse_args ()
    return args

def init_config ():
    config_options = {task: {} for task in TASKS}
    config_options["validity"]["preproc_callback"] = preprocess_valid_relation_prediction
    config_options["validity"]["num_labels"] = len (VALID_LABELS)
    config_options["validity"]["label_space"] = VALID_LABELS
    config_options["validity"]["label_field"] = "valid_relation"

    #config_options["spatial"]["preproc_callback"] = preprocess_spatial_relation_prediction
    #config_options["spatial"]["num_labels"] = len (SPATIAL_RELATION_LABELS)
    #config_options["spatial"]["label_space"] = SPATIAL_RELATION_LABELS
    #config_options["spatial"]["label_field"] = "spatial_relation"

    return config_options

def main (args):
    # Layout the configurations
    config_options = init_config ()
    os.makedirs (args.models_dir, exist_ok=True)
    os.makedirs (args.results_dir, exist_ok=True)

    # set the device
    device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")

    # Train and test relation predictor
    predictor = BERTRelationPrediction (model_name=args.pretrained_model_name,
                                        dims=args.dims,
                                        n_labels=config_options[args.task_name]["num_labels"],
                                        n_hidden=args.num_hidden,
                                        device=device,
                                        lr=args.learning_rate,
                                        labels=config_options[args.task_name]["label_space"])
    
    predictor.load_data (args.train_data_file,
                         args.train_labels_file,
                         args.dev_data_file,
                         args.dev_labels_file, 
                         preprocess=config_options[args.task_name]["preproc_callback"],
                         num_training_examples=args.num_training_examples)
    
    #print (np.sum(["<char>" in item.split() for item in predictor.dev_df[args.text_field].values]))
    #return
	
    predictor.start_training (num_epochs=args.num_epochs, 
                              text_field=args.text_field,
                              label_field=config_options[args.task_name]["label_field"], 
                              verbose=True)
    
    predictor.save (os.path.join (args.models_dir, f'{args.task_name}.pt'),
                    os.path.join (args.results_dir, f'{args.task_name}.examples.tsv'),
                    os.path.join (args.results_dir, f'{args.task_name}.dynamics.tsv'))

if __name__ == "__main__":
    main (readArgs ())