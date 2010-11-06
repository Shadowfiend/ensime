package org.ensime.protocol

import java.io._
import org.ensime.config.{ ProjectConfig, DebugConfig, ReplConfig }
import org.ensime.debug.{ DebugUnit, DebugSourceLinePairs }
import org.ensime.model._
import org.ensime.server._
import org.ensime.util._
import org.ensime.util.SExp._
import scala.actors._
import scala.tools.nsc.util.{ Position }
import scala.tools.refactoring.common.Change
import scala.util.parsing.input
import net.liftweb.json.JsonDSL._
import net.liftweb.json.JsonAST._
import net.liftweb.json.{Printer, JsonParser}

object SwankJsonProtocol extends SwankJsonProtocol {}

trait SwankJsonProtocol extends Protocol {
  implicit def jObject2WireFormat(obj: JObject): WireFormat = new WireFormat {
    def toWireString = Printer.compact(render(obj))
  }

  import SwankProtocol._
  import ProtocolConst._

  val PROTOCOL_VERSION: String = "0.0.1"

  val SERVER_NAME: String = "ENSIMEserver"

  private var outPeer: Actor = null;
  private var rpcTarget: RPCTarget = null;

  def peer = outPeer

  def setOutputActor(peer: Actor) { outPeer = peer }

  def setRPCTarget(target: RPCTarget) { this.rpcTarget = target }

  // Handle reading / writing of messages

  def writeMessage(value: WireFormat, out: Writer) {
    val data: String = value.toWireString
    val header: String = String.format("%06x", int2Integer(data.length))
    val msg = header + data
    println("Writing: " + msg)
    out.write(msg)
    out.flush()
  }

  private def fillArray(in: java.io.Reader, a: Array[Char]) {
    var n = 0
    var l = a.length
    var charsRead = 0;
    while (n < l) {
      charsRead = in.read(a, n, l - n)
      if (charsRead == -1) {
        throw new EOFException("End of file reached in socket reader.");
      } else {
        n += charsRead
      }
    }
  }

  private val headerBuf = new Array[Char](6);

  def readMessage(in: java.io.Reader): WireFormat = {
    fillArray(in, headerBuf)
    val msglen = Integer.valueOf(new String(headerBuf), 16).intValue()
    if (msglen > 0) {
      JsonParser.parse(in)
    } else {
      throw new IllegalStateException("Empty message read from socket!")
    }
  }

  def sendBackgroundMessage(code: Int, detail:Option[String]) {
    val msg: JObject = ("background-message" -> ("code" -> code) ~ ("detail" -> detail.getOrElse("")))
    sendMessage(msg)
  }

  def handleIncomingMessage(msg: Any) {
    msg match {
      case json: JValue => handleMessageForm(json)
      case _ => System.err.println("WTF: Unexpected message: " + msg)
    }
  }

  private def handleMessageForm(json: JObject) {
    json match {
      case JObject(List(JField("swank-rpc", JObject(List(JField(_, JInt(callId)), _))))) => {
        handleEmacsRex(form, callId)
      }
      case _ => {
        sendProtocolError(ErrUnrecognizedForm, Some(Printer.pretty(render(json))))
      }
    }
  }

  val emacsCharOffset = 1

  private def handleEmacsRex(form: JObject, callId: Int) {
    form match {
      case SExpList(SymbolAtom(name) :: rest) => {
        try {
          handleRPCRequest(name, form, callId)
        } catch {
          case e: Throwable =>
          {
            e.printStackTrace(System.err)
            sendRPCError(ErrExceptionInRPC, Some(e.getMessage), callId)
          }
        }
      }
      case _ => {
        sendRPCError(
          ErrMalformedRPC,
          Some("Expecting leading symbol in: " + form),
          callId)
      }
    }
  }

  private def handleRPCRequest(callType: String, form: SExp, callId: Int) {

    println("\nHandling RPC: " + form)

    def oops = sendRPCError(ErrMalformedRPC, Some("Malformed " + callType + " call: " + form), callId)

    callType match {
      case "swank:connection-info" => {
        sendConnectionInfo(callId)
      }
      case "swank:init-project" => {
        form match {
          case SExpList(head ::(conf: SExpList) :: body) => {
            val config = ProjectConfig.fromSExp(conf)
            rpcTarget.rpcInitProject(config, callId)
          }
          case _ => oops
        }
      }
      case "swank:peek-undo" => {
        rpcTarget.rpcPeekUndo(callId)
      }
      case "swank:exec-undo" => {
        form match {
          case SExpList(head ::(IntAtom(id)) :: body) => {
            rpcTarget.rpcExecUndo(id, callId)
          }
          case _ => oops
        }
      }
      case "swank:repl-config" => {
        rpcTarget.rpcReplConfig(callId)
      }
      case "swank:builder-init" => {
        rpcTarget.rpcBuilderInit(callId)
      }
      case "swank:builder-add-files" => {
        form match {
          case SExpList(head :: SExpList(filenames) :: body) => {
            val files = filenames.map(_.toString)
            rpcTarget.rpcBuilderAddFiles(files, callId)
          }
          case _ => oops
        }
      }
      case "swank:builder-update-files" => {
        form match {
          case SExpList(head :: SExpList(filenames) :: body) => {
            val files = filenames.map(_.toString)
            rpcTarget.rpcBuilderUpdateFiles(files, callId)
          }
          case _ => oops
        }
      }
      case "swank:builder-remove-files" => {
        form match {
          case SExpList(head :: SExpList(filenames) :: body) => {
            val files = filenames.map(_.toString)
            rpcTarget.rpcBuilderRemoveFiles(files, callId)
          }
          case _ => oops
        }
      }
      case "swank:debug-config" => {
        rpcTarget.rpcDebugConfig(callId)
      }
      case "swank:debug-unit-info" => {
        form match {
          case SExpList(head :: StringAtom(sourceName) :: IntAtom(line) :: StringAtom(packPrefix) :: body) => {
            rpcTarget.rpcDebugUnitInfo(sourceName, line, packPrefix, callId)
          }
          case _ => oops
        }
      }

      case "swank:debug-class-locs-to-source-locs" => {
        form match {
          case SExpList(head :: SExpList(pairs) :: body) => {
            val nameLinePairs = pairs.flatMap {
              case SExpList((classname: StringAtom) ::(line: IntAtom) :: body) => {
                Some(classname.toString, line.value)
              }
              case _ => Some("", -1)
            }
            rpcTarget.rpcDebugClassLocsToSourceLocs(nameLinePairs, callId)
          }
          case _ => oops
        }
      }
      case "swank:typecheck-file" => {
        form match {
          case SExpList(head :: StringAtom(file) :: body) => {
            rpcTarget.rpcTypecheckFile(file, callId)
          }
          case _ => oops
        }
      }
      case "swank:typecheck-all" => {
        rpcTarget.rpcTypecheckAll(callId)
      }
      case "swank:scope-completion" => {
        form match {
          case SExpList(head :: StringAtom(file) :: IntAtom(point) :: StringAtom(prefix) :: BooleanAtom(constructor) :: body) => {
            rpcTarget.rpcScopeCompletion(file, point, prefix, constructor, callId)
          }
          case _ => oops
        }
      }
      case "swank:type-completion" => {
        form match {
          case SExpList(head :: StringAtom(file) :: IntAtom(point) :: StringAtom(prefix) :: body) => {
            rpcTarget.rpcTypeCompletion(file, point, prefix, callId)
          }
          case _ => oops
        }
      }
      case "swank:import-suggestions" => {
        form match {
          case SExpList(head :: StringAtom(file) :: IntAtom(point) :: SExpList(names) :: body) => {
            rpcTarget.rpcImportSuggestions(file, point,
              names.map(_.toString).toList, callId)
          }
          case _ => oops
        }
      }
      case "swank:package-member-completion" => {
        form match {
          case SExpList(head :: StringAtom(path) :: StringAtom(prefix) :: body) => {
            rpcTarget.rpcPackageMemberCompletion(path, prefix, callId)
          }
          case _ => oops
        }
      }
      case "swank:inspect-type-at-point" => {
        form match {
          case SExpList(head :: StringAtom(file) :: IntAtom(point) :: body) => {
            rpcTarget.rpcInspectTypeAtPoint(file, point, callId)
          }
          case _ => oops
        }
      }
      case "swank:inspect-type-by-id" => {
        form match {
          case SExpList(head :: IntAtom(id) :: body) => {
            rpcTarget.rpcInspectTypeById(id, callId)
          }
          case _ => oops
        }
      }
      case "swank:symbol-at-point" => {
        form match {
          case SExpList(head :: StringAtom(file) :: IntAtom(point) :: body) => {
            rpcTarget.rpcSymbolAtPoint(file, point, callId)
          }
          case _ => oops
        }
      }
      case "swank:type-by-id" => {
        form match {
          case SExpList(head :: IntAtom(id) :: body) => {
            rpcTarget.rpcTypeById(id, callId)
          }
          case _ => oops
        }
      }
      case "swank:type-by-name" => {
        form match {
          case SExpList(head :: StringAtom(name) :: body) => {
            rpcTarget.rpcTypeByName(name, callId)
          }
          case _ => oops
        }
      }
      case "swank:type-by-name-at-point" => {
        form match {
          case SExpList(head :: StringAtom(name) :: StringAtom(file) :: IntAtom(point) :: body) => {
            rpcTarget.rpcTypeByNameAtPoint(name, file, point, callId)
          }
          case _ => oops
        }
      }
      case "swank:call-completion" => {
        form match {
          case SExpList(head :: IntAtom(id) :: body) => {
            rpcTarget.rpcCallCompletion(id, callId)
          }
          case _ => oops
        }
      }
      case "swank:type-at-point" => {
        form match {
          case SExpList(head :: StringAtom(file) :: IntAtom(point) :: body) => {
            rpcTarget.rpcTypeAtPoint(file, point, callId)
          }
          case _ => oops
        }
      }
      case "swank:inspect-package-by-path" => {
        form match {
          case SExpList(head :: StringAtom(path) :: body) => {
            rpcTarget.rpcInspectPackageByPath(path, callId)
          }
          case _ => oops
        }
      }

      case "swank:perform-refactor" => {
        form match {
          case SExpList(head :: IntAtom(procId) :: SymbolAtom(tpe) ::(params: SExp) :: body) => {
            rpcTarget.rpcPerformRefactor(Symbol(tpe), procId,
              listOrEmpty(params).toSymbolMap, callId)
          }
          case _ => oops
        }
      }

      case "swank:exec-refactor" => {
        form match {
          case SExpList(head :: IntAtom(procId) :: SymbolAtom(tpe) :: body) => {
            rpcTarget.rpcExecRefactor(Symbol(tpe), procId, callId)
          }
          case _ => oops
        }
      }

      case "swank:cancel-refactor" => {
        form match {
          case SExpList(head :: IntAtom(procId) :: body) => {
            rpcTarget.rpcCancelRefactor(procId, callId)
          }
          case _ => oops
        }
      }

      case "swank:format-source" => {
        form match {
          case SExpList(head :: SExpList(filenames) :: body) => {
            val files = filenames.map(_.toString)
            rpcTarget.rpcFormatFiles(files, callId)
          }
          case _ => oops
        }
      }

      case other => {
        sendRPCError(
          ErrUnrecognizedRPC,
          Some("Unknown :swank-rpc call: " + other),
          callId)
      }
    }
  }

  def listOrEmpty(list: SExp): SExpList = {
    list match {
      case l: SExpList => l
      case _ => SExpList(List())
    }
  }

  def sendRPCAckOK(callId: Int) {
    sendRPCReturn(true, callId)
  }

  def sendRPCReturn(value: WireFormat, callId: Int) {
    value match {
      case sexp: SExp =>
      {
        sendMessage(SExp(
          key(":return"),
          SExp(key(":ok"), sexp),
          callId))
      }
      case _ => throw new IllegalStateException("Not a SExp: " + value)
    }
  }

  def sendRPCError(code: Int, detail: Option[String], callId: Int) {
    sendMessage(SExp(
      key(":return"),
      SExp(key(":abort"),
        code,
        detail.map(strToSExp).getOrElse(NilAtom())
      ),
      callId))
  }

  def sendProtocolError(code: Int, detail: Option[String]){
    sendMessage(
      SExp(
        key(":reader-error"),
        code,
        detail.map(strToSExp).getOrElse(NilAtom())
      ))
  }

  /*
  * A sexp describing the server configuration, per the Swank standard.
  */
  def sendConnectionInfo(callId: Int) = {
    val inf = ("pid" -> null) ~
      ("server-implementation" -> ("name" -> SERVER_NAME)) ~
      ("machine" -> null) ~
      ("features" -> null) ~
      ("version" -> PROTOCOL_VERSION)
    sendRPCReturn(inf, callId)
  }

  def sendCompilerReady() = sendMessage(JObject(List(JField("compiler-ready", JBool(true)))))

  def sendTypeCheckResult(notelist: NoteList) = {
    sendMessage(JObject(List(JField("typecheck-result", toWF(notelist)))))
  }

  object JValueConversion {

    implicit def posToJValue(pos: Position): JValue = {
      if (pos.isDefined) {
        ("file" -> pos.source.path) ~ ("offset" -> pos.point + emacsCharOffset)
      } else {
        JNull
      }
    }

  }

  import JValueConversion._

  def toWF(config: ProjectConfig): JValue = {
    ("project-name" -> config.name) ~ ("source-roots" -> config.sourceRoots.map(_.getPath))
  }

  def toWF(config: ReplConfig): JValue = {
    ("classpath" -> config.classpath)
  }

  def toWF(config: DebugConfig): JValue = {
    ("classpath" -> config.classpath) ~ ("sourcepath" -> config.sourcepath)
  }

  def toWF(unit: DebugUnit): JValue = {
    ("full-name" -> unit.classQualName) ~
      ("package" -> unit.packageName) ~
      ("start-line" -> unit.startLine) ~
      ("end-line" -> unit.endLine)
  }

//  def toWF(value: Boolean): JValue = {
//    if (value) TruthAtom()
//    else NilAtom()
//  }
//
//  def toWF(value: Null): JValue = {
//    NilAtom()
//  }
//
//  def toWF(value: String): JValue = {
//    StringAtom(value)
//  }

  def toWF(value: DebugSourceLinePairs): JValue = {
    
    JArray(value.pairs.map( p => JObject(List(JField(p._1, p._2)))).toList)
  }

  def toWF(note: Note): JValue = {
    ("severity" -> note.friendlySeverity) ~
    ("msg" -> note.msg) ~
    ("beg" -> note.beg + emacsCharOffset) ~
    ("end" -> note.end + emacsCharOffset) ~
    ("line" -> note.line) ~
    ("col" -> note.col) ~
    ("file" -> note.file)
  }

  def toWF(notelist: NoteList): JValue = {
    val NoteList(isFull, notes) = notelist
    ("is-full" -> isFull) ~ ("notes" -> notes.map(toWF))
  }

//  def toWF(values: Iterable[WireFormat]): JValue = {
//    SExpList(values.map(ea => ea.asInstanceOf[SExp]))
//  }

  def toWF(value: SymbolInfoLight): JValue = {
    ("name" -> value.name) ~
    ("type-sig" -> value.tpeSig) ~
    ("type-id" -> value.tpeId) ~
    ("is-callable" -> value.isCallable)
  }

  def toWF(value: PackageMemberInfoLight): JValue = {
    ("name" -> value.name)
  }

  def toWF(value: SymbolInfo): JValue = {
    ("name" -> value.name) ~
    ("type" -> toWF(value.tpe)) ~
    ("decl-pos" -> value.declPos) ~
    ("is-callable" -> value.isCallable)
  }

  def toWF(value: NamedTypeMemberInfoLight): JValue = {
    ("name" -> value.name) ~
    ("type-sig" -> value.tpeSig) ~
    ("type-id" -> value.tpeId) ~
    ("is-callable" -> value.isCallable)
  }

  def toWF(value: NamedTypeMemberInfo): JValue = {
    ("name" -> value.name) ~
    ("type" -> toWF(value.tpe)) ~
    ("pos" -> value.pos) ~
    ("decl-as" -> value.declaredAs)
  }

  def toWF(value: EntityInfo): JValue = {
    value match {
      case value: PackageInfo => toWF(value)
      case value: TypeInfo => toWF(value)
      case value: NamedTypeMemberInfo => toWF(value)
      case value: NamedTypeMemberInfoLight => toWF(value)
      case value => throw new IllegalStateException("Unknown EntityInfo: " + value)
    }
  }

  def toWF(value: TypeInfo): JValue = {
    value match {
      case value: ArrowTypeInfo =>
      {
        ("name" -> value.name) ~
        ("type-id" -> value.id) ~
        ("arrow-type" -> true) ~
        ("result-type" -> toWF(value.resultType)) ~
        ("params-sections" -> value.paramsSection.map(sect => sect.map(toWF).toList).toList)
      }
      case value: TypeInfo =>
      {
        ("name" -> value.name) ~
        ("type-id" -> value.id) ~
        ("full-name" -> value.fullName) ~
        ("decl-as" -> value.declaredAs) ~
        ("type-args" -> value.args.map(toWF).toList) ~
        ("members" -> value.members.map(toWF).toList) ~
        ("pos" -> value.pos) ~
        ("outer-type-id" ~ value.outerTypeId.map(JInt(_)).getOrElse(JNull))
      }
      case value => throw new IllegalStateException("Unknown TypeInfo: " + value)
    }
  }

  def toWF(value: PackageInfo): JValue = {
    ("name" -> value.name) ~ ("info-type" -> "package") ~ ("full-name" -> value.fullName) ~ ("members" -> value.members.map(toWF).toList)
  }

  def toWF(value: CallCompletionInfo): JValue = {
    ("result-type" -> value.resultType) ~
    ("params-sections" -> (value.paramSections.map { sect =>
      sect.map(pair => JObject(List(JField(pair._1, toWF(pair._2))))).toList
    }).toList)
  }

  def toWF(value: InterfaceInfo): JValue = {
    ("type" -> toWF(value.tpe)) ~ ("via-view" -> value.viaView.getOrElse(""))
  }

  def toWF(value: TypeInspectInfo): JValue = {
    ("type" -> toWF(value.tpe)) ~
    ("info-type" -> "typeInspect") ~
    ("companion-id" -> value.companionId.map(JInt(_)).getOrElse(JNull))
    ("interfaces" -> value.supers.map(toWF).toList)
  }

  def toWF(value: RefactorFailure): JValue = {
    ("procedure-id" -> value.procedureId) ~
    ("status" -> "failure") ~
    ("reason" -> value.message)
  }

  def toWF(value: RefactorEffect): JValue = {
    ("procedure-id" -> value.procedureId) ~
    ("refactor-type" -> value.refactorType) ~
    ("status" -> "success") ~
    ("changes" -> value.changes.map(changeToWF).toList)
  }

  def toWF(value: RefactorResult): JValue = {
    ("procedure-id" -> value.procedureId) ~
    ("refactor-type" -> value.refactorType) ~
    ("touched-files" -> value.touched.map(_.getAbsolutePath).toList)
  }

  def toWF(value: ImportSuggestions): JValue = {
    JArray(value.symLists.map(l -> JArray(l.map(toWF).toList)).toList)
  }

  def toWF(value: Undo): JValue = {
    ("id" -> value.id) ~ ("changes" -> value.changes.map(changeToWF)) ~ ("summary" -> value.summary)
  }

  def toWF(value: UndoResult): JValue = {
    ("id" -> value.id) ~ ("touched-files" -> value.touched.toList)
  }

  private def changeToWF(ch: Change): JValue = {
    ("file" -> ch.file.path) ~
      ("text" -> ch.text) ~
      ("from" -> ch.from.emacsCharOffset) ~
      ("to" -> ch.to + emacsCharOffset)
  }

}