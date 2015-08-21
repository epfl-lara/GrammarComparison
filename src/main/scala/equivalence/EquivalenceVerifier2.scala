//  //TODO: this is an interesting transformation 
//  //We can check if this can be used in combination with A and B transformations
//  /*def reduceUsingHypotheses(context: Set[Relation])(rel: Relation): TransformationResult = {
//
//    val BinaryOperator(op, lhs, rhs) = rel
//    if (lhs.size > 1 || rhs.size > 1)
//      NotApplicable(List(rel))
//    val lform = lhs(0)
//    val rform = rhs(0)
//
//    def containsContext(ctxl: SententialForm, ctxr: SententialForm) = {
//      val lindex = lform.indexOfSlice(ctxl)
//      val rindex = rform.indexOfSlice(ctxr)
//      if (lindex >= 0 && rindex >= 0)
//        Some(lindex, rindex)
//      else
//        None
//    }
//
//    //try to find the largest hypotheses match (sum of the sizes the hypothesis should be 
//    // greater than the existing context)
//    var hypothesisSize = 0
//    var newl = List[Symbol]()
//    var newr = List[Symbol]()
//    context.takeWhile(_ => hypothesisSize < lform.size + rform.size) foreach {
//      case BinaryOperator(ctxop, List(ctxl), List(ctxr)) if (ctxl.size + ctxr.size > hypothesisSize) && impliesOp(ctxop, op) =>
//
//        val res1 = containsContext(ctxl, ctxr)
//        val (indices, lfrag, rfrag) = if (res1.isDefined)
//          (res1, ctxl, ctxr)
//        else if (ctxop == Equals)
//          (containsContext(ctxr, ctxl), ctxr, ctxl) //equals is commutative
//        else
//          (res1, ctxl, ctxr)
//        if (indices.isDefined) {
//          val Some((lindex, rindex)) = indices
//          newl = lform.take(lindex) ++ lform.drop(lindex + lfrag.size)
//          newr = rform.take(rindex) ++ rform.drop(rindex + rfrag.size)
//          hypothesisSize = lfrag.size + rfrag.size
//        }
//      case _ => ;
//    }
//    if (hypothesisSize == 0)
//      NotApplicable(List(rel))
//    else {
//      val newrel = op(List(newl), List(newr))
//      All(List(newrel))
//    }
//  }
//*/
//
