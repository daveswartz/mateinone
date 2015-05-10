package mateinone.evaluators

import mateinone.Board

trait Evaluator {
  def evaluate(b: Board): Int
}
