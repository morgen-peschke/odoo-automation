package peschke.odoo.algebras

import cats.Monad
import cats.data.NonEmptyList
import cats.syntax.all._
import org.typelevel.log4cats.LoggerFactory
import peschke.odoo.models.CheckedTemplate.PickingName
import peschke.odoo.models.Template.{Entry, EntryLabel, Location, LocationDest, MoveName, MoveTemplate, PickingNameTemplate, PickingTemplate, Tag}
import peschke.odoo.models.{TagFilter, Template, TemplateFilters}

trait TemplateFilterer[F[_]] {
  def keepEntry(entry: Entry, filters: TemplateFilters): F[Boolean]

  def keepPicking(picking: PickingTemplate, name: PickingName, filters: TemplateFilters): F[Boolean]

  def keepMove(move: MoveTemplate, pickingName: PickingName, filters: TemplateFilters): F[Boolean]
}
object TemplateFilterer      {
  def apply[F[_]](implicit TF: TemplateFilterer[F]): TF.type = TF

  def default[F[_]: Monad: LoggerFactory]: TemplateFilterer[F] =
    new TemplateFilterer[F] {
      private val logger = LoggerFactory[F].getLoggerFromClass(classOf[TemplateChecker[F]])

      def keepEntry(entry: Entry, filters: TemplateFilters): F[Boolean] =
        (
          checkLabel(entry.label, filters),
          checkTags(entry.tags, entry.label, filters)
        ).mapN(_ && _)

      def keepPicking(picking: PickingTemplate, name: PickingName, filters: TemplateFilters): F[Boolean] =
        (
          checkName(PickingNameTemplate.raw(picking.pickingName), filters),
          checkName(PickingName.raw(name), filters),
          checkSource(picking.locationId, name, filters),
          checkDestination(picking.locationDestId, name, filters)
        ).mapN(_ && _ && _ && _)

      def keepMove(move: MoveTemplate, pickingName: PickingName, filters: TemplateFilters): F[Boolean] =
        checkProduct(move.productId, pickingName, move.name, filters)

      private def checkLabel(label: EntryLabel, filter: TemplateFilters): F[Boolean] =
        filter
          .labelFilter
          .textFilter
          .matches(label.string)
          .traverse(f => logger.info(show"$label matched filter: $f").unlessA(f.isTrivial))
          .map(_.isDefined)
          .flatTap {
            case true  =>
              logger
                .info(s"$label passed label filter check")
                .unlessA(filter.labelFilter.textFilter.isTrivial)
            case false =>
              logger.info(s"Skipping entry because of label filters: $label")
          }

      private def checkTags(tags: List[Tag], label: EntryLabel, filter: TemplateFilters): F[Boolean] =
        filter.tagFilter match {
          case TagFilter.Exists(tf) =>
            NonEmptyList
              .fromList(tags.flatMap(t => tf.matches(t.string).tupleLeft(t))) match {
              case Some(matches) =>
                val tag = if (matches.length === 1) "tag" else "tags"
                logger
                  .info {
                    matches
                      .map { case t -> f =>
                        show"[$t] $f"
                      }
                      .mkString_(show"Keeping $label because $tag matched filter\n  ", "\n  ", "")
                  }
                  .unlessA(tf.isTrivial)
                  .as(true)
              case None          =>
                logger.info(show"Skipping $label because no tag matched filter\n  $tf").as(false)
            }
          case TagFilter.ForAll(tf) =>
            NonEmptyList.fromList(tags.flatMap(t => tf.fails(t.string).tupleLeft(t))) match {
              case Some(matches) =>
                val tag = if (matches.length === 1) "tag" else "tags"
                logger
                  .info {
                    matches
                      .map { case t -> f =>
                        show"[$t] $f"
                      }
                      .mkString_(show"Skipping $label because $tag failed filter\n  ", "\n  ", "")
                  }
                  .unlessA(tf.isTrivial)
                  .as(false)
              case None          =>
                logger.info(show"Keeping $label because no tag matched filter\n  $tf").as(true)
            }
        }

      private def checkName(pickingName: String, filter: TemplateFilters): F[Boolean] =
        filter
          .pickingNameFilter
          .textFilter
          .matches(pickingName)
          .traverse(f => logger.info(show"$pickingName matched filter: $f").unlessA(f.isTrivial))
          .map(_.isDefined)
          .flatTap {
            case true  =>
              logger
                .info(s"Passed name filter: $pickingName")
                .unlessA(filter.pickingNameFilter.textFilter.isTrivial)
            case false => logger.info(s"Skipping because of name filters: $pickingName")
          }

      private def checkSource(location: Location, pickingName: PickingName, filter: TemplateFilters): F[Boolean] =
        location.nameOpt match {
          case None       =>
            logger
              .info(s"Skipping filter check in $pickingName because the location name was not known: $location")
              .unlessA(filter.productFilter.textFilter.isTrivial)
              .as(true)
          case Some(name) =>
            filter
              .sourceLocationFilter
              .textFilter
              .matches(name)
              .traverse(f => logger.info(show"Location $name matched filter: $f").unlessA(f.isTrivial))
              .map(_.isDefined)
              .flatTap {
                case true  =>
                  logger
                    .info(s"Location passed filter check in $pickingName: $name")
                    .unlessA(filter.pickingNameFilter.textFilter.isTrivial)
                case false => logger.info(s"Skipping $pickingName because of location filters: $name")
              }
        }

      private def checkDestination
        (location: LocationDest, pickingName: PickingName, filter: TemplateFilters)
        : F[Boolean] =
        location.nameOpt match {
          case None       =>
            logger
              .info(s"Skipping filter check in $pickingName because the location_dest name was not known: $location")
              .unlessA(filter.productFilter.textFilter.isTrivial)
              .as(true)
          case Some(name) =>
            filter
              .destinationLocationFilter
              .textFilter
              .matches(name)
              .traverse(f => logger.info(show"Location_dest $name matched filter: $f").unlessA(f.isTrivial))
              .map(_.isDefined)
              .flatTap {
                case true  =>
                  logger
                    .info(s"Location_dest passed filter check in $pickingName: $name")
                    .unlessA(filter.pickingNameFilter.textFilter.isTrivial)
                case false => logger.info(s"Skipping $pickingName because of location_dest filters: $name")
              }
        }

      private def checkProduct
        (product: Template.Product, pickingName: PickingName, moveName: MoveName, filter: TemplateFilters)
        : F[Boolean] =
        product.nameOpt match {
          case None       =>
            logger
              .info(
                s"Skipping filter check for $moveName in $pickingName because the product name was not known: $product"
              )
              .unlessA(filter.productFilter.textFilter.isTrivial)
              .as(true)
          case Some(name) =>
            filter
              .productFilter
              .textFilter
              .matches(name)
              .traverse(f => logger.info(show"Product $name matched filter: $f").unlessA(f.isTrivial))
              .map(_.isDefined)
              .flatTap {
                case true  =>
                  logger
                    .info(s"Product passed filter check for $moveName in $pickingName: $name")
                    .unlessA(filter.pickingNameFilter.textFilter.isTrivial)
                case false => logger.info(s"Skipping $moveName in $pickingName because of product filters: $name")
              }
        }

    }
}
