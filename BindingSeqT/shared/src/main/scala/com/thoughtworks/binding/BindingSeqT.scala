package com.thoughtworks.binding

import scalaz.StreamT
import scala.concurrent.Future
import scalaz.Monad
import scalaz.Nondeterminism
import scalaz.Applicative
import scalaz.StreamT.Skip
import scalaz.StreamT.Done
import scalaz.StreamT.Yield
import scalaz.StreamT.Step
import scalaz.IList
import scalaz.-\/
import scalaz.\/-
import scalaz.Monoid
import scalaz.ICons
import scalaz.INil
import scalaz.Isomorphism.IsoFunctor
import scalaz.Isomorphism.IsoFunctorTemplate
import scalaz.MonadPlus
import scalaz.IsomorphismMonadPlus
import scalaz.Functor
import scalaz.Free
import scalaz.Traverse
import scalaz.FingerTree
import scalaz.Reducer
import scalaz.UnitReducer
import scalaz.DList

object BindingSeqT:
  sealed trait Patch[A]
  object Patch:
    // TODO: Support move items
    // final case class Move[A](oldIndex: Int, offset: Int, moveCount: Int) extends Patch[A]
    final case class Splice[A](index: Int, deleteCount: Int, newItems: Iterable[A]) extends Patch[A]

  given [M[_]](using M: Nondeterminism[M]): Monad[[a] =>> BindingSeqT[M, a]] with
    def point[A](a: => A): BindingSeqT[M, A] =
      Applicative[[a] =>> StreamT[M, a]].point(Patch.Splice[A](0, 0, List(a)))
    def bind[A, B](fa: BindingSeqT[M, A])(f: A => BindingSeqT[M, B]): BindingSeqT[M, B] =
      val toStepB = { (a: A) =>
        f(a).step
      }
      type ChooseSubSteps = M[
        (
            (
                SourceIndex,
                MappedIndex,
                Step[
                  Patch[B],
                  BindingSeqT[M, B]
                ]
            ),
            scalaz.IList[
              M[
                (
                    SourceIndex,
                    MappedIndex,
                    Step[
                      Patch[B],
                      BindingSeqT[M, B]
                    ]
                )
              ]
            ]
        )
      ]
      type ChooseSubStepsOption = Option[ChooseSubSteps]
      type MappedIndex = Int
      type SourceIndex = Int
      type MappedSize = Int
      type SourceSize = Int
      type SizeMapping = (SourceSize, MappedSize)

      def mapICons[A, B](icons: ICons[A])(f: A => B): ICons[B] = {
        icons.map(f).asInstanceOf[ICons[B]]
      }

      def bindSubSteps(
          subSteps: ICons[(SourceIndex, MappedIndex, M[StreamT.Step[Patch[B], BindingSeqT[M, B]]])]
      )(
          // TODO: optimize to reuse chooseSubStepsOption when subSteps are not changed
          chooseSubSteps: ChooseSubSteps = M
            .chooseAny(subSteps.map { case (sourceIndex, mappedIndex, mStepB) =>
              M.map(mStepB)((sourceIndex, mappedIndex, _))
            })
            .get
      ): M[Step[Patch[B], BindingSeqT[M, B]]] =
        M.map(chooseSubSteps) { case ((sourceIndex, mappedIndex, stepB), mSubSteps) =>
          stepB match
            case Yield(patch @ Patch.Splice(index, deleteCount, newItems), s) =>
              Yield(
                patch.copy(index = index + mappedIndex),
                () =>
                  StreamT(
                    bindSubSteps(
                      mapICons(subSteps) { subStep =>
                        if (subStep._1 > sourceIndex) {
                          subStep.copy(_2 = subStep._2 - deleteCount + newItems.size)
                        } else if (subStep._1 == sourceIndex) {
                          subStep.copy(_3 = s().step)
                        } else {
                          subStep
                        }
                      }
                    )()
                  )
              )
            case Skip(s) =>
              Skip(() =>
                StreamT(
                  bindSubSteps(
                    mapICons(subSteps) { subStep =>
                      if (subStep._1 == sourceIndex) {
                        subStep.copy(_3 = s().step)
                      } else {
                        subStep
                      }
                    }
                  )()
                )
              )
            case Done() =>
              subSteps.filter(_._1 != sourceIndex) match
                case restStepBs: ICons[_] =>
                  Skip(() =>
                    StreamT(
                      bindSubSteps(
                        restStepBs
                      )()
                    )
                  )
                case INil() =>
                  Done()
        }

      def bindStep(
          mStepA: M[StreamT.Step[Patch[A], BindingSeqT[M, A]]],
          sizeMappingTree: FingerTree[SizeMapping, SizeMapping],
          subSteps: IList[(SourceIndex, MappedIndex, M[StreamT.Step[Patch[B], BindingSeqT[M, B]]])]
      )(
          // TODO: optimize to reuse chooseSubStepsOption when subSteps are not changed
          chooseSubStepsOption: ChooseSubStepsOption = M.chooseAny(subSteps.map {
            case (sourceIndex, mappedIndex, mStepB) =>
              M.map(mStepB)((sourceIndex, mappedIndex, _))
          })
      ): M[Step[Patch[B], BindingSeqT[M, B]]] =
        def handleStepA(
            stepA: StreamT.Step[Patch[A], BindingSeqT[M, A]]
        ): Step[Patch[B], BindingSeqT[M, B]] =
          stepA match
            case Yield(Patch.Splice(index, deleteCount, newItems), s) =>
              // TODO: Avoid unnecessary FingerTree operations to optimize the performance
              val (left, notLeft) = sizeMappingTree.split { case (sourceIndex, _) =>
                sourceIndex < index
              }
              val (deleted, right) = notLeft.split { case (sourceIndex, _) =>
                sourceIndex < deleteCount
              }
              // TODO: Use measureMonoid once https://github.com/scalaz/scalaz/pull/2234 is merged
              val (`deleteCount`, numberOfMappedItemsDeleted) = deleted.measure.getOrElse((0, 0))

              val numberOfSourceItemsAdd = newItems.size
              val newSizeMappingTree = newItems.foldLeft(left) { (tree, _) =>
                tree :+ (1, 0)
              } <++> right

              val numberOfSourceItemsIncreased = numberOfSourceItemsAdd - deleteCount
              val (`index`, mappedIndex) = left.measure.getOrElse((0, 0))
              val newSubSteps = subSteps.foldRight(
                IList.empty[(SourceSize, MappedSize, M[StreamT.Step[Patch[B], BindingSeqT[M, B]]])]
              ) { (subStep, tail) =>
                val (sourceIndex, mappedIndex, mSubStep) = subStep
                val adjustedTail = if (sourceIndex >= index + deleteCount) {
                  (sourceIndex + numberOfSourceItemsAdd, mappedIndex - numberOfMappedItemsDeleted, mSubStep) :: tail
                } else if (sourceIndex >= index) {
                  tail
                } else {
                  subStep :: tail
                }
                if (sourceIndex == index) {
                  newItems.view.zipWithIndex.foldRight(adjustedTail) { case ((newItem, i), adjustedTail) =>
                    (index + i, mappedIndex, toStepB(newItem)) :: adjustedTail
                  }
                } else {
                  adjustedTail
                }
              }
              val next = () => StreamT(bindStep(s().step, newSizeMappingTree, newSubSteps)())
              if (numberOfMappedItemsDeleted > 0) {
                Yield(Patch.Splice(mappedIndex, numberOfMappedItemsDeleted, Nil), next)
              } else {
                Skip(next)
              }
            case Skip(s) =>
              Skip(() => StreamT(bindStep(s().step, sizeMappingTree, subSteps)()))
            case Done() =>
              subSteps match
                case nonEmptySubSteps: ICons[_] =>
                  Skip(() => StreamT(bindSubSteps(nonEmptySubSteps)()))
                case INil() =>
                  Done()
        chooseSubStepsOption match
          case None =>
            M.map(mStepA)(handleStepA(_))
          case Some(chooseSubSteps) =>
            M.map(M.choose(mStepA, chooseSubSteps)) {
              case -\/((stepA, _)) =>
                handleStepA(stepA)
              case \/-((mStepA, ((sourceIndex, mappedIndex, stepB), _))) =>
                stepB match
                  case Yield(patch @ Patch.Splice(index, deleteCount, newItems), s) =>
                    val (left, (1, mappedSize), right) = sizeMappingTree.split1 { case (nodeSourceIndex, _) =>
                      nodeSourceIndex < sourceIndex
                    }
                    val newSizeMappingTree = left.add1((1, mappedSize - deleteCount + newItems.size), right)
                    Yield(
                      patch.copy(index = index + mappedIndex),
                      () =>
                        StreamT(
                          bindStep(
                            mStepA,
                            newSizeMappingTree,
                            subSteps.map { subStep =>
                              if (subStep._1 > sourceIndex) {
                                subStep.copy(_2 = subStep._2 - deleteCount + newItems.size)
                              } else if (subStep._1 == sourceIndex) {
                                subStep.copy(_3 = s().step)
                              } else {
                                subStep
                              }
                            }
                          )()
                        )
                    )
                  case Skip(s) =>
                    Skip(() =>
                      StreamT(
                        bindStep(
                          mStepA,
                          sizeMappingTree,
                          subSteps.map { subStep =>
                            if (subStep._1 == sourceIndex) {
                              subStep.copy(_3 = s().step)
                            } else {
                              subStep
                            }
                          }
                        )()
                      )
                    )
                  case Done() =>
                    Skip(() =>
                      StreamT(
                        bindStep(
                          mStepA,
                          sizeMappingTree,
                          subSteps.filter(_._1 != sourceIndex)
                        )()
                      )
                    )
            }
      import scalaz.std.tuple.given
      import scalaz.std.anyVal.given
      import scalaz.std.option.given
      StreamT(bindStep(fa.step, FingerTree.empty(Reducer.identityReducer), IList.empty)())

opaque type BindingSeqT[M[_], A] = StreamT[M, BindingSeqT.Patch[A]]
