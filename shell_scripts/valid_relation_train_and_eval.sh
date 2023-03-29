python ../scripts/valid_relation_train_and_eval.py \
       --pretrained-model-name bert-base-cased \
       --annotated-data-file ../data/annotations/final_annotations/final_annotations.v1.pickle \
       --num-epochs 15 \
       --window-size 100 \
       --text-field context_100 \
       --label-field 'Valid Relation' \
       --model-path ../data/experiments/bad_categories/model_checkpoints/trained.v1.pt \
       --num-labels 2 \
       --test-ids-file ../data/annotations/final_annotations/common_ids.txt